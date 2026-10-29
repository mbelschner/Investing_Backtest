# =============================================================================
# 02_preise_yahoo.R  --  Preisdaten von Yahoo Finance holen
# =============================================================================
#
# WAS PASSIERT HIER?  Drei Schritte:
#
#   1. ZUORDNUNG RIC -> Yahoo-Ticker, per ISIN-Suche bei Yahoo. Ergebnis
#      landet in data/raw/ticker_map.csv und wird nur einmal gesucht.
#      Falsche Treffer kannst du dort von Hand korrigieren (quelle = "manuell").
#
#   2. DOWNLOAD mit Cache: ein Request je Ticker fuer die ganze Historie,
#      eine .rds je Ticker in data/cache/. Pause zwischen den Requests,
#      bei 429 ("zu viele Anfragen") bremst der Downloader sich selbst.
#      Symbole, die Yahoo nicht kennt, werden nicht jedes Mal neu probiert.
#
#   3. PRUEFUNG jeder Reihe, bevor sie ins Panel darf:
#      - Waehrung passt nicht zum Markt  -> anderes Wertpapier, raus
#      - delisteter Titel, dessen Reihe erst NACH dem Delisting beginnt oder
#        lange danach weiterlaeuft -> Yahoo hat das Kuerzel neu vergeben
#        (INFO war IHS Markit, ist heute eine andere Firma), raus
#      - delistete Titel werden am Delisting-Datum abgeschnitten.
#
# Offline arbeiten (nur Cache, kein Netz): CFG$yh_offline <- TRUE
#
# Ergebnis: data/proc/prices.rds  (long-Format: id, date, ohlcv, adjclose)
#           data/proc/universe.rds (Spalte yahoo aktualisiert)
#           output/price_check.csv (welche Reihe warum verworfen wurde)
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
    # Vorsicht: London notiert in PENCE, Yahoo schreibt dann "GBp". Ein
    # toupper() machte daraus "GBP" -- und der Umsatz war 100-fach zu hoch.
    # Pence heissen bei uns "GBX".
    cur <- r$meta$currency %||% NA_character_
    dt[, currency := if (identical(cur, "GBp")) "GBX" else toupper(cur)]
    dt[, ccy_src := cur]
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
  if (isTRUE(CFG$yh_offline)) {
    msg("Offline-Modus: kein Download, es wird nur der Cache benutzt.")
    return(invisible(stt))
  }
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
# 4. ZUORDNUNG RIC -> YAHOO PER ISIN
# =============================================================================
# Die Faustregel ric_to_yahoo() aus 00 trifft nur die Boersen-Endung sicher.
# Yahoos Suchfeld kennt aber die ISIN. Wir fragen je ISIN einmal nach und
# nehmen den Treffer, dessen Endung zur Boerse des RIC passt (BASFn.DE ->
# Endung ".DE" -> BAS.DE). Ohne passenden Treffer bleibt die Faustregel.

yh_search_isin <- function(isin) {
  resp <- tryCatch(
    request("https://query2.finance.yahoo.com/v1/finance/search") |>
      req_url_query(q = isin, quotesCount = 10, newsCount = 0) |>
      req_headers(`User-Agent` = CFG$yh_user_agent) |>
      req_timeout(20) |>
      req_error(is_error = function(resp) FALSE) |>
      req_perform(),
    error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200L) return(character(0))
  js <- tryCatch(fromJSON(resp_body_string(resp), simplifyVector = FALSE),
                 error = function(e) NULL)
  q <- js$quotes
  if (!length(q)) return(character(0))
  sym  <- vapply(q, function(e) e$symbol    %||% NA_character_, "")
  type <- vapply(q, function(e) e$quoteType %||% NA_character_, "")
  sym[type %in% "EQUITY" & !is.na(sym)]
}

#' Den Kandidaten waehlen, dessen Endung zur Faustregel passt
pick_symbol <- function(cands, rule) {
  if (!length(cands) || is.na(rule)) return(NA_character_)
  suffix <- if (grepl("\\.", rule)) sub("^.*(\\.[^.]+)$", "\\1", rule) else ""
  ok <- if (nzchar(suffix)) endsWith(cands, suffix) else !grepl("\\.", cands)
  if (any(ok)) cands[ok][1] else NA_character_
}

map_path <- function() file.path(CFG$dir_raw, "ticker_map.csv")

#' ticker_map.csv laden und um neue RICs ergaenzen
#'
#' Spalten: id (RIC), isin, yahoo, quelle ("isin", "regel" oder "manuell").
#' Bereits vorhandene Zeilen werden nie neu gesucht -- wer korrigieren will,
#' traegt den richtigen Ticker ein und setzt quelle auf "manuell".
build_ticker_map <- function(universe) {
  map <- if (file.exists(map_path()))
    fread(map_path(), colClasses = "character") else
    data.table(id = character(), isin = character(),
               yahoo = character(), quelle = character())

  neu <- universe[!id %in% map$id, .(id, isin, rule = yahoo)]
  if (nrow(neu) && isTRUE(CFG$yh_offline)) {
    msg("Offline: ", nrow(neu), " RICs ohne Zuordnung, nehme die Faustregel ",
        "(werden beim naechsten Online-Lauf gesucht).")
    return(rbind(map, neu[, .(id, isin, yahoo = rule, quelle = "regel")]))
  }
  if (nrow(neu)) {
    msg("ISIN-Suche fuer ", nrow(neu), " RICs ...")
    yh_prime()
    pb <- txtProgressBar(min = 0, max = nrow(neu), style = 3)
    for (i in seq_len(nrow(neu))) {
      hit <- NA_character_
      if (!is.na(neu$isin[i]) && nzchar(neu$isin[i])) {
        hit <- pick_symbol(yh_search_isin(neu$isin[i]), neu$rule[i])
        Sys.sleep(.yh_session$sleep)
      }
      map <- rbind(map, data.table(
        id = neu$id[i], isin = neu$isin[i],
        yahoo  = if (is.na(hit)) neu$rule[i] else hit,
        quelle = if (is.na(hit)) "regel" else "isin"))
      if (i %% 50L == 0L) fwrite(map, map_path())
      setTxtProgressBar(pb, i)
    }
    close(pb)
    fwrite(map, map_path())
  }
  msg("Ticker-Zuordnung: ", paste(names(table(map$quelle)), table(map$quelle),
                                  sep = " = ", collapse = " | "))
  map[]
}

# =============================================================================
# 5. CACHE ZU EINEM PANEL ZUSAMMENBAUEN UND PRUEFEN
# =============================================================================

#' Alle .rds-Dateien aus dem Cache zu einer langen Tabelle verbinden
#'
#' @param map data.table mit Spalten id (RIC) und yahoo (Yahoo-Ticker)
assemble_prices <- function(map) {

  map <- unique(map[!is.na(yahoo), .(id, yahoo)], by = "id")
  files <- cache_path(map$yahoo)
  keep  <- file.exists(files)
  msg("Panel bauen aus ", sum(keep), " von ", nrow(map), " Cache-Dateien.")

  lst <- lapply(which(keep), function(k) {
    x <- tryCatch(readRDS(files[k]), error = function(e) NULL)
    if (is.null(x) || nrow(x) == 0) return(NULL)
    # Altbestand im Cache: dort steht fuer London "GBP", obwohl die Kurse in
    # Pence sind (siehe yh_fetch_one). Neue Dateien tragen ccy_src.
    if (!"ccy_src" %in% names(x) && endsWith(map$yahoo[k], ".L") &&
        identical(x$currency[1], "GBP"))
      x[, currency := "GBX"]
    if ("ccy_src" %in% names(x)) x[, ccy_src := NULL]
    x[, id := map$id[k]]
  })

  p <- rbindlist(lst, use.names = TRUE, fill = TRUE)
  setcolorder(p, c("id", "date"))
  setorder(p, id, date)
  unique(p, by = c("id", "date"))
}

#' Reihen verwerfen, die nicht zum RIC gehoeren koennen
#'
#' Gibt das bereinigte Panel zurueck und schreibt output/price_check.csv.
check_prices <- function(p, universe) {
  chk <- p[, .(currency = currency[1], first_px = min(date),
               last_px = max(date)), by = id]
  chk <- merge(universe[, .(id, name, market, yahoo, delist_date)], chk,
               by = "id", all.x = TRUE)
  chk[, ccy_erwartet := unname(MARKET_CCY[market])]
  chk[, grund := fcase(
    is.na(first_px), "keine Kurse",
    !is.na(ccy_erwartet) & currency != ccy_erwartet, "Waehrung passt nicht",
    !is.na(delist_date) & first_px > delist_date, "Kuerzel neu vergeben",
    !is.na(delist_date) & last_px > delist_date + 60, "laeuft nach Delisting weiter",
    default = "ok")]

  msg("--- Pruefung der Kursreihen ---")
  print(chk[, .N, by = grund][order(-N)])
  bad <- chk[!grund %in% c("ok", "keine Kurse")]
  if (nrow(bad))
    print(bad[, .(id, yahoo, name, market, currency, first_px, last_px,
                  delist_date, grund)])
  fwrite(chk, file.path(CFG$dir_out, "price_check.csv"))

  p <- p[id %in% chk[grund == "ok", id]]
  # Tote Titel am Delisting-Datum abschneiden: was danach kommt, gehoert
  # nicht mehr zu diesem Unternehmen.
  p <- merge(p, universe[, .(id, delist_date)], by = "id", all.x = TRUE)
  p <- p[is.na(delist_date) | date <= delist_date]
  p[, delist_date := NULL]
  setorder(p, id, date)

  # Der Survivorship-Check: wie viele der delisteten Titel haben Kurse?
  tot <- chk[!is.na(delist_date)]
  msg("Delistete Titel mit brauchbaren Kursen: ", tot[grund == "ok", .N],
      " von ", nrow(tot), ". Der Rest fehlt im Panel -- das ist die ",
      "Groesse des verbleibenden Survivorship-Problems.")
  msg("Preis-Panel: ", format(nrow(p), big.mark = ".", decimal.mark = ","), " Zeilen, ",
      uniqueN(p$id), " Ticker, ", format(min(p$date)), " bis ",
      format(max(p$date)))
  p[]
}

# =============================================================================
# 6. WECHSELKURSE
# =============================================================================
# Volumen mal Kurs ergibt den Tagesumsatz in LOKALER Waehrung. Fuer die
# Liquiditaetsdezile brauchen wir eine gemeinsame Waehrung (USD).

fetch_fx <- function(currencies) {
  currencies <- setdiff(unique(toupper(currencies[!is.na(currencies)])),
                        c("USD", "GBX"))
  msg("FX holen fuer: ", paste(currencies, collapse = ", "))
  out <- lapply(currencies, function(cur) {
    sym <- paste0(cur, "USD=X")   # KRWUSD=X = wie viel USD kostet 1 KRW
    r <- yh_fetch_one(sym, CFG$date_start, CFG$date_end)
    Sys.sleep(.yh_session$sleep)
    if (!r$ok) { msg("  FX fehlgeschlagen: ", sym, " (", r$status, ")"); return(NULL) }
    data.table(currency = cur, date = r$data$date, fx_usd = r$data$close)
  })
  fx <- rbindlist(out)
  fx <- rbind(fx, data.table(currency = "USD", date = sort(unique(fx$date)),
                             fx_usd = 1))
  setorder(fx, currency, date)
  fx[]
}

# Pence = Pfund / 100
add_gbx <- function(fx) {
  if ("GBX" %in% fx$currency || !"GBP" %in% fx$currency) return(fx)
  rbind(fx, fx[currency == "GBP"][, `:=`(currency = "GBX", fx_usd = fx_usd / 100)])
}

# =============================================================================
# HAUPTLAUF
# =============================================================================

if (run_main()) {

  universe <- readRDS(file.path(CFG$dir_proc, "universe.rds"))

  # 1. Zuordnung
  map <- build_ticker_map(universe)
  universe[map, on = "id", yahoo := i.yahoo]
  saveRDS(universe, file.path(CFG$dir_proc, "universe.rds"))

  # 2. Download (zum Testen erst mal klein: fetch_prices(..., max_new = 100))
  fetch_prices(universe$yahoo)

  # 3. Panel bauen und pruefen
  prices <- check_prices(assemble_prices(universe[, .(id, yahoo)]), universe)
  saveRDS(prices, file.path(CFG$dir_proc, "prices.rds"))

  # 4. Wechselkurse
  fx_file <- file.path(CFG$dir_proc, "fx.rds")
  fx <- if (file.exists(fx_file)) readRDS(fx_file) else fetch_fx(prices$currency)
  saveRDS(add_gbx(fx), fx_file)

  # 5. Abdeckung je Markt: fehlt ein Land fast komplett, ist es ein
  #    Zuordnungsproblem und keine Datenluecke.
  cov <- universe[, .(ticker = .N,
                      mit_kursen = sum(id %in% prices$id),
                      davon_delistet = sum(!is.na(delist_date)),
                      delistet_mit_kursen = sum(!is.na(delist_date) & id %in% prices$id)),
                  by = market][order(-ticker)]
  print(cov)
  print(load_status()[, .N, by = status][order(-N)])
}
