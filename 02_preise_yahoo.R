# =============================================================================
# 02_preise_yahoo.R  --  Preisdaten von Yahoo Finance holen
# =============================================================================
#
# WAS PASSIERT HIER?
# Wir brauchen fuer rund 2.400 Ticker die komplette Tageshistorie seit 2019.
# Naiv gemacht sind das zehntausende Requests und Yahoo sperrt dich aus.
#
# Vier Regeln, damit das nicht passiert:
#
#   1. EIN Request pro Ticker fuer die GESAMTE Historie.
#      Der Chart-Endpunkt liefert 7 Jahre Tagesdaten in einem Aufruf.
#      Nicht pro Jahr oder pro Monat schleifen.
#
#   2. CACHE auf der Platte. Jeder Ticker landet als .rds in data/cache/.
#      Beim zweiten Lauf wird nur geholt, was fehlt. Ein abgebrochener Lauf
#      kostet nichts -- einfach neu starten, er macht dort weiter.
#
#   3. PAUSE zwischen den Requests (Default 0,35 s = ca. 3 Requests/Sekunde)
#      und automatisches Hochfahren der Pause, sobald Yahoo mit 429
#      ("Too Many Requests") antwortet. Der Downloader bremst sich selbst.
#
#   4. TOTE TICKER MERKEN. Ein delistetes Papier, das Yahoo nicht kennt,
#      wird einmal probiert und dann in der Statusdatei als "hoffnungslos"
#      vermerkt. Nicht bei jedem Lauf 200 mal ins Leere greifen.
#
# Laufzeit erster Durchlauf: ca. 15-20 Minuten fuer 2.400 Ticker.
# Jeder weitere Lauf: wenige Minuten.
#
# Ergebnis: data/proc/prices.rds  (long-Format: id, date, ohlcv, adjclose)
# =============================================================================

source("00_config.R")

# =============================================================================
# 1. EIN EINZELNER ABRUF
# =============================================================================

# Yahoo mag es, wenn man vorher einmal "normal" auf die Seite geht und
# Cookies einsammelt. Wir machen das einmal pro R-Session.
.yh_session <- new.env(parent = emptyenv())
.yh_session$primed <- FALSE
.yh_session$sleep  <- CFG$yh_sleep

yh_prime <- function() {
  if (isTRUE(.yh_session$primed)) return(invisible(TRUE))
  try({
    request("https://finance.yahoo.com/") |>
      req_headers(`User-Agent` = CFG$yh_user_agent) |>
      req_timeout(15) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform()
  }, silent = TRUE)
  .yh_session$primed <- TRUE
  invisible(TRUE)
}

#' Tagesdaten eines Symbols von Yahoo holen
#'
#' @param symbol  Yahoo-Ticker, z.B. "005930.KS"
#' @param from,to Datumsgrenzen
#' @return Liste mit $ok (TRUE/FALSE), $data (data.table) und $status
yh_fetch_one <- function(symbol, from = CFG$date_start, to = CFG$date_end) {
  
  url <- sprintf(
    "https://query2.finance.yahoo.com/v8/finance/chart/%s",
    utils::URLencode(symbol, reserved = TRUE)
  )
  
  for (attempt in seq_len(CFG$yh_max_retry)) {
    
    resp <- tryCatch({
      request(url) |>
        req_url_query(
          period1  = as.integer(as.POSIXct(from, tz = "UTC")),
          period2  = as.integer(as.POSIXct(to + 1, tz = "UTC")),
          interval = "1d",
          events   = "div,split",
          includeAdjustedClose = "true"
        ) |>
        req_headers(`User-Agent` = CFG$yh_user_agent,
                    `Accept` = "application/json") |>
        req_timeout(30) |>
        req_error(is_error = function(resp) FALSE) |>   # Fehler selbst behandeln
        req_perform()
    }, error = function(e) NULL)
    
    # --- Netzwerkfehler: kurz warten, nochmal ---
    if (is.null(resp)) {
      Sys.sleep(2^attempt)
      next
    }
    
    st <- resp_status(resp)
    
    # --- 429 = zu schnell. Pause dauerhaft erhoehen und warten. ---
    if (st == 429L) {
      .yh_session$sleep <- min(.yh_session$sleep * 1.6, 5)
      Sys.sleep(5 * attempt)
      next
    }
    
    # --- 404 / 401 = Symbol existiert bei Yahoo nicht. Kein Retry. ---
    if (st %in% c(400L, 401L, 403L, 404L)) {
      return(list(ok = FALSE, data = NULL, status = paste0("http_", st)))
    }
    
    # --- 5xx = Yahoo hat gerade Probleme ---
    if (st >= 500L) {
      Sys.sleep(2^attempt)
      next
    }
    
    # --- 200: JSON auspacken ---
    js <- tryCatch(
      fromJSON(resp_body_string(resp), simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(js) || is.null(js$chart) || is.null(js$chart$result) ||
        length(js$chart$result) == 0) {
      return(list(ok = FALSE, data = NULL, status = "empty"))
    }
    
    r  <- js$chart$result[[1]]
    ts <- r$timestamp
    if (is.null(ts) || length(ts) == 0)
      return(list(ok = FALSE, data = NULL, status = "no_bars"))
    
    q  <- r$indicators$quote[[1]]
    ac <- if (!is.null(r$indicators$adjclose))
      r$indicators$adjclose[[1]]$adjclose else NULL
    
    # Hilfsfunktion: Yahoo liefert JSON-Nulls -> in NA umwandeln
    unl <- function(x, n) {
      if (is.null(x)) return(rep(NA_real_, n))
      v <- vapply(x, function(e) if (is.null(e)) NA_real_ else as.numeric(e),
                  numeric(1))
      v
    }
    n <- length(ts)
    
    # Zeitstempel sind UTC-Sekunden zum Handelsstart. Fuer das richtige
    # Handelsdatum muessen wir in die Boersen-Zeitzone umrechnen, sonst
    # rutscht Tokio um einen Tag.
    tz <- r$meta$exchangeTimezoneName %||% "UTC"
    dates <- as.Date(as.POSIXct(unlist(ts), origin = "1970-01-01", tz = "UTC"),
                     tz = tz)
    
    dt <- data.table(
      date     = dates,
      open     = unl(q$open,   n),
      high     = unl(q$high,   n),
      low      = unl(q$low,    n),
      close    = unl(q$close,  n),
      volume   = unl(q$volume, n),
      adjclose = if (is.null(ac)) unl(q$close, n) else unl(ac, n)
    )
    dt <- dt[!is.na(date) & !is.na(adjclose) & adjclose > 0]
    dt <- unique(dt, by = "date")
    setorder(dt, date)
    
    # Waehrung als echte Spalte mitschreiben. Brauchen wir spaeter, um den
    # Tagesumsatz in USD umzurechnen (Liquiditaetsfilter).
    dt[, currency := toupper(r$meta$currency %||% NA_character_)]
    return(list(ok = TRUE, data = dt, status = "ok"))
  }
  
  list(ok = FALSE, data = NULL, status = "retry_exhausted")
}

# =============================================================================
# 2. CACHE-VERWALTUNG
# =============================================================================

cache_path <- function(symbol) {
  # Dateinamen entschaerfen: "BRK-B" und "005930.KS" sind ok, aber wir
  # ersetzen alles, was auf Windows/Linux Aerger macht.
  safe <- gsub("[^A-Za-z0-9._-]", "_", symbol)
  file.path(CFG$dir_cache, paste0(safe, ".rds"))
}

status_path <- function() file.path(CFG$dir_proc, "price_status.rds")

load_status <- function() {
  p <- status_path()
  if (file.exists(p)) return(readRDS(p))
  data.table(yahoo = character(), status = character(),
             last_try = as.Date(character()),
             last_full = as.Date(character()),
             first_date = as.Date(character()),
             last_date = as.Date(character()),
             n_rows = integer(), n_fail = integer())
}

# =============================================================================
# 3. DER DOWNLOADER
# =============================================================================

#' Preise fuer eine Liste von Yahoo-Symbolen holen (mit Cache)
#'
#' @param symbols  Zeichenvektor mit Yahoo-Tickern
#' @param force    TRUE = Cache ignorieren und alles neu laden
#' @param max_new  Obergrenze fuer Requests in diesem Lauf. Praktisch, wenn
#'                 du erst mal 200 Ticker testen willst.
fetch_prices <- function(symbols, force = FALSE, max_new = Inf) {
  
  symbols <- unique(symbols[!is.na(symbols) & nzchar(symbols)])
  stt <- load_status()
  setkey(stt, yahoo)
  yh_prime()
  
  today <- Sys.Date()
  n_req <- 0L
  n_ok  <- 0L
  n_skip<- 0L
  
  msg("Downloader startet: ", length(symbols), " Symbole.")
  pb <- txtProgressBar(min = 0, max = length(symbols), style = 3)
  
  for (i in seq_along(symbols)) {
    sym <- symbols[i]
    setTxtProgressBar(pb, i)
    
    row <- stt[.(sym)]
    have_row <- nrow(row) == 1L && !is.na(row$status)
    # NA-sichere Kopien: ein NA in einer if-Bedingung wirft in R einen Fehler.
    st_prev <- if (have_row) row$status else NA_character_
    nf_prev <- if (have_row && !is.na(row$n_fail)) row$n_fail else 0L
    cp <- cache_path(sym)
    have_file <- file.exists(cp)
    
    # --- Ueberspringen? ------------------------------------------------------
    if (!force && have_row) {
      
      # a) Symbol existiert bei Yahoo nachweislich nicht -> nie wieder probieren
      if (isTRUE(st_prev %in% c("http_404", "http_400")) && nf_prev >= 1L) {
        n_skip <- n_skip + 1L; next
      }
      # b) mehrfach fehlgeschlagen -> hoechstens einmal pro Woche neu probieren
      if (isTRUE(st_prev != "ok") && nf_prev >= 3L &&
          isTRUE(!is.na(row$last_try) && today - row$last_try < 7)) {
        n_skip <- n_skip + 1L; next
      }
      # c) Cache ist frisch genug
      if (isTRUE(st_prev == "ok") && have_file) {
        
        # Wie lange ist der letzte vollstaendige Download her?
        age_full <- if (is.na(row$last_full)) Inf else
          as.numeric(today - row$last_full)
        
        # Ist die Reihe tot? Erkennungsmerkmal: schon beim letzten Download
        # lag der letzte Kurstag deutlich in der Vergangenheit. Ein lebender
        # Titel hat last_date immer dicht an last_full.
        series_dead <- !is.na(row$last_date) && !is.na(row$last_full) &&
          as.numeric(row$last_full - row$last_date) > 10
        
        refresh_after <- if (series_dead) CFG$yh_dead_refresh_days else
          CFG$yh_refresh_days
        
        if (age_full < refresh_after) { n_skip <- n_skip + 1L; next }
      }
    }
    
    if (n_req >= max_new) break
    
    # --- Holen ---------------------------------------------------------------
    # Wir laden immer die VOLLE Historie, nicht nur die neuen Tage.
    # Grund: adjclose wird bei jeder Dividende rueckwirkend fuer die gesamte
    # Vergangenheit neu skaliert. Ein inkrementelles Anhaengen wuerde einen
    # Bruch in der Renditereihe erzeugen. Ein Request kostet dasselbe,
    # egal ob 30 oder 1.800 Tage -- also nehmen wir alles.
    res <- yh_fetch_one(sym, CFG$date_start, CFG$date_end)
    n_req <- n_req + 1L
    Sys.sleep(.yh_session$sleep)
    
    if (res$ok && nrow(res$data) > 0) {
      saveRDS(res$data, cp)
      n_ok <- n_ok + 1L
      newrow <- data.table(
        yahoo = sym, status = "ok", last_try = today, last_full = today,
        first_date = min(res$data$date), last_date = max(res$data$date),
        n_rows = nrow(res$data), n_fail = 0L)
    } else {
      nf <- nf_prev + 1L
      newrow <- data.table(
        yahoo = sym, status = res$status, last_try = today,
        last_full = if (have_row) row$last_full else as.Date(NA),
        first_date = if (have_row) row$first_date else as.Date(NA),
        last_date  = if (have_row) row$last_date  else as.Date(NA),
        n_rows = if (have_row) row$n_rows else 0L, n_fail = nf)
    }
    
    stt <- rbind(stt[yahoo != sym], newrow, fill = TRUE)
    setkey(stt, yahoo)   # Key geht beim rbind verloren, sofort neu setzen
    
    # Status alle 50 Ticker sichern, damit ein Abbruch nichts kostet
    if (n_req %% 50L == 0L) saveRDS(stt, status_path())
  }
  
  close(pb)
  setkey(stt, yahoo)
  saveRDS(stt, status_path())
  
  msg("Requests: ", n_req, " | erfolgreich: ", n_ok,
      " | uebersprungen (Cache): ", n_skip,
      " | aktuelle Pause: ", round(.yh_session$sleep, 2), "s")
  invisible(stt)
}

# =============================================================================
# 4. CACHE ZU EINEM PANEL ZUSAMMENBAUEN
# =============================================================================

#' Alle .rds-Dateien aus dem Cache zu einer langen Tabelle verbinden
#'
#' @param map data.table mit Spalten id (RIC) und yahoo (Yahoo-Ticker)
assemble_prices <- function(map) {
  
  map <- unique(map[!is.na(yahoo), .(id, yahoo)], by = "id")
  files <- cache_path(map$yahoo)
  keep  <- file.exists(files)
  msg("Panel bauen aus ", sum(keep), " von ", nrow(map), " Cache-Dateien.")
  
  lst <- vector("list", sum(keep))
  ids <- map$id[keep]
  fl  <- files[keep]
  
  for (k in seq_along(fl)) {
    x <- tryCatch(readRDS(fl[k]), error = function(e) NULL)
    if (is.null(x) || nrow(x) == 0) next
    x[, id := ids[k]]
    lst[[k]] <- x
  }
  
  p <- rbindlist(lst, use.names = TRUE, fill = TRUE)
  p <- p[!is.na(id)]
  setcolorder(p, c("id", "date"))
  setorder(p, id, date)
  p <- unique(p, by = c("id", "date"))
  
  msg("Preis-Panel: ", format(nrow(p), big.mark = "."), " Zeilen, ",
      uniqueN(p$id), " Ticker, ",
      format(min(p$date)), " bis ", format(max(p$date)))
  p[]
}

# =============================================================================
# 5. WECHSELKURSE  (frueher in 03_universum_pit.R)
# =============================================================================
# Volumen mal Kurs ergibt den Tagesumsatz in LOKALER Waehrung. 5 Mio KRW sind
# aber keine 5 Mio USD. Fuer die Liquiditaetsdezile in der Diagnose brauchen
# wir eine gemeinsame Waehrung, also holen wir eine Handvoll FX-Reihen mit.
# Das sind ~15 zusaetzliche Requests.

fetch_fx <- function(currencies) {
  currencies <- setdiff(unique(toupper(currencies[!is.na(currencies)])), "USD")
  msg("FX holen fuer: ", paste(currencies, collapse = ", "))

  out <- vector("list", length(currencies))
  for (k in seq_along(currencies)) {
    cur <- currencies[k]
    sym <- paste0(cur, "USD=X")   # KRWUSD=X = wie viel USD kostet 1 KRW
    r <- yh_fetch_one(sym, CFG$date_start, CFG$date_end)
    Sys.sleep(.yh_session$sleep)
    if (r$ok) out[[k]] <- data.table(currency = cur, date = r$data$date,
                                     fx_usd = r$data$close)
    else msg("  FX fehlgeschlagen: ", sym, " (", r$status, ")")
  }
  fx <- rbindlist(out)
  if (nrow(fx)) {
    d <- sort(unique(fx$date))
    fx <- rbind(fx, data.table(currency = "USD", date = d, fx_usd = 1))
  }
  setorder(fx, currency, date)
  fx[]
}

# =============================================================================
# HAUPTLAUF
# =============================================================================

if (run_main()) {

  universe <- readRDS(file.path(CFG$dir_proc, "universe.rds"))

  # Zum Testen erst mal klein anfangen:
  #   fetch_prices(universe$yahoo, max_new = 100)
  fetch_prices(universe$yahoo)

  # Panel nur neu bauen, wenn sich im Cache etwas getan hat.
  prices_file <- file.path(CFG$dir_proc, "prices.rds")
  cache_files <- list.files(CFG$dir_cache, pattern = "\\.rds$", full.names = TRUE)
  newest_cache <- if (length(cache_files)) max(file.mtime(cache_files)) else as.POSIXct(NA)

  if (file.exists(prices_file) && !is.na(newest_cache) &&
      file.mtime(prices_file) > newest_cache) {
    msg("prices.rds ist aktueller als der Cache -- wird wiederverwendet.")
    prices <- readRDS(prices_file)
  } else {
    prices <- assemble_prices(universe[, .(id, yahoo)])
    saveRDS(prices, prices_file)
  }

  # --- FX ---------------------------------------------------------------
  fx_file <- file.path(CFG$dir_proc, "fx.rds")
  if (file.exists(fx_file)) {
    fx <- readRDS(fx_file)
    msg("FX aus Cache: ", uniqueN(fx$currency), " Waehrungen.")
  } else {
    fx <- fetch_fx(unique(prices$currency))
    saveRDS(fx, fx_file)
  }

  # ===========================================================================
  # ABDECKUNGSPRUEFUNG: haben wir fuer jedes EREIGNIS auch Kurse?
  # ===========================================================================
  # Das ist der Check, der frueher gefehlt hat. Ein Ticker "hat Kurse" reicht
  # nicht -- entscheidend ist, ob rund um JEDEN Berichtstermin genug Handels-
  # tage vorliegen. Ohne Vorlauf keine Vor-Ereignis-Volatilitaet, ohne
  # Nachlauf kein CAR60. Fehlt beides systematisch bei denselben Titeln,
  # verlierst du nicht zufaellig Daten, sondern ein ganzes Marktsegment.

  fund_file <- file.path(CFG$dir_proc, "fundamentals.rds")
  if (!file.exists(fund_file)) {
    msg("Hinweis: fundamentals.rds fehlt -- erst 01_parameter.R laufen lassen, ",
        "dann ist hier auch die Ereignis-Abdeckung zu sehen.")
  } else {

    fund <- readRDS(fund_file)
    setDT(prices)

    # a) Ticker-Ebene
    cov_id <- prices[, .(n_px = .N, first_px = min(date), last_px = max(date)),
                     by = id]
    cov_id <- merge(universe[, .(id, name, market, yahoo, delist_date)],
                    cov_id, by = "id", all.x = TRUE)
    cov_id[, hat_kurse := !is.na(n_px)]
    cov_id[, hat_events := id %in% fund[!is.na(report_date), unique(id)]]

    msg("--- Abdeckung Ticker-Ebene ---")
    print(cov_id[, .N, by = .(hat_events, hat_kurse)][order(-N)])

    fehlt <- cov_id[hat_events == TRUE & hat_kurse == FALSE]
    msg("Ticker mit Quartalsberichten, aber OHNE Kurse: ", nrow(fehlt))
    if (nrow(fehlt)) print(head(fehlt[order(market), .(id, yahoo, name, market)], 20))
    msg("  -> Haeufigste Ursache: RIC-zu-Yahoo-Uebersetzung. Pruefe die ",
        "Boersenkuerzel oben in RIC_EXCHANGE_MAP.")

    # b) Ereignis-Ebene: Handelstage vor und nach jedem Berichtstermin.
    # Wir zaehlen HANDELSTAGE, nicht Kalendertage -- fuer ein Ereignisfenster
    # von 60 Boersentagen ist nur die Zahl der tatsaechlichen Kurse relevant.
    ev <- fund[!is.na(report_date), .(id, period_end, report_date)]

    px_ct <- prices[, .(id, date)]
    setorder(px_ct, id, date)
    px_ct[, t_idx := seq_len(.N), by = id]
    px_ct[, n_id  := .N, by = id]
    setkey(px_ct, id, date)

    # roll = -Inf sucht vorwaerts: der erste Handelstag AM oder NACH dem
    # Berichtstermin. Genau der ist spaeter Ereignistag 0.
    hit <- px_ct[ev[, .(id, date = report_date)], on = .(id, date), roll = -Inf]
    ev[, t0     := hit$t_idx]
    ev[, n_ges  := hit$n_id]
    ev[, hat_kurse := !is.na(t0)]
    ev[, n_pre  := t0 - 1L]          # Handelstage vor dem Ereignis
    ev[, n_post := n_ges - t0]       # Handelstage danach

    ev[, ok_event := hat_kurse & !is.na(n_pre) & n_pre >= 60 &
                     !is.na(n_post) & n_post >= 60]

    msg("--- Abdeckung Ereignis-Ebene ---")
    msg("Berichtstermine gesamt          : ", nrow(ev))
    msg("davon mit Kursreihe             : ", sum(ev$hat_kurse))
    msg("davon mit >=60 Handelstagen davor: ", sum(ev$n_pre >= 60, na.rm = TRUE))
    msg("davon mit >=60 Handelstagen danach: ", sum(ev$n_post >= 60, na.rm = TRUE))
    msg("VOLL AUSWERTBAR                 : ", sum(ev$ok_event), " (",
        round(100 * mean(ev$ok_event), 1), "%)")

    # Wo genau verlieren wir die Ereignisse? Nach Markt aufschluesseln --
    # wenn ein Land komplett fehlt, ist es ein Uebersetzungsproblem und
    # keine Datenluecke.
    ev2 <- merge(ev, universe[, .(id, market)], by = "id", all.x = TRUE)
    print(ev2[, .(events = .N,
                  pct_mit_kurs = round(100 * mean(hat_kurse), 1),
                  pct_auswertbar = round(100 * mean(ok_event), 1)),
              by = market][order(pct_auswertbar)])

    # Und ueber die Zeit: am aktuellen Rand fehlt der Nachlauf zwangslaeufig.
    # Das ist normal. Wenn die Quote aber in der MITTE der Historie einbricht,
    # stimmt etwas nicht.
    ev2[, jahr := as.integer(format(report_date, "%Y"))]
    print(ev2[, .(events = .N, pct_auswertbar = round(100 * mean(ok_event), 1)),
              by = jahr][order(jahr)])

    fwrite(ev2[, .(id, market, period_end, report_date, hat_kurse,
                   n_pre, n_post, ok_event)],
           file.path(CFG$dir_out, "price_coverage.csv"))
    msg("Detailtabelle: output/price_coverage.csv")
  }

  # --- Downloadstatus ---------------------------------------------------
  stt <- load_status()
  print(stt[, .N, by = status][order(-N)])
}
