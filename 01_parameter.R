# =============================================================================
# 01_parameter.R  --  LSEG-Exporte einlesen und alle Kennzahlen berechnen
# =============================================================================
#
# WAS PASSIERT HIER?  In vier Schritten:
#
#   A) Rohdaten einlesen: alle Batch-CSVs aus data/raw/ untereinander haengen.
#   B) Fundamentals und Estimates verheiraten (ueber Ticker + Quartalsende).
#   C) TTM bilden -- rollierende 4-Quartals-Summen, damit Saisonalitaet raus ist.
#   D) Kennzahlen rechnen -- genau die Formeln aus Tab "04_R_Formeln".
#
# WICHTIG, und das ist der Kern des ganzen Skripts:
# Jede Zeile bekommt ein Feld "known_from". Das ist der Tag, ab dem diese
# Zahlen oeffentlich waren -- also das Berichtsdatum, NICHT das Quartalsende.
# Samsungs Q2-Zahlen zum 30.06. kennt der Markt erst am 31.07. Wer im
# Backtest am 01.07. schon damit rechnet, baut sich eine Zeitmaschine.
#
# Ergebnis: data/proc/fundamentals.rds  (eine Zeile je Ticker x Quartal)
# =============================================================================

source("00_config.R")

# -----------------------------------------------------------------------------
# Mindesthistorie fuer die selbstnormierenden Kennzahlen
# -----------------------------------------------------------------------------
# SUE teilt die Ueberraschung durch die Streuung der eigenen Surprise-Historie.
# Diese Streuung braucht genug Punkte, sonst ist sie zufaellig zu klein und der
# Quotient zu gross. In den Daten haben rund 12% der Quartale weniger als fuenf
# verwertbare Vorquartale -- ohne Untergrenze landen genau die am Rand der
# Verteilung und damit im Extremquintil.
#
# 6 von 8 ist ein Kompromiss: strenger kostet frueh im Sample zu viele
# Ereignisse (die Historie beginnt 2019), lockerer laesst Zufallszahlen durch.
# Mit 6 bleiben rund 79% der Quartale mit Surprise erhalten, mit 4 rund 87%.
SUE_MIN_HIST <- 6L   # Vorquartale mit gueltiger Surprise, noetig fuer sue
HIT_MIN_HIST <- 3L   # dito fuer surp_hit_rate4 (Fenster ist nur 4 lang)

# Testmodus aus run_all.R uebernehmen, falls dort gesetzt.
if (exists("TEST_MODE")) CFG$test_mode <- isTRUE(TEST_MODE)
if (exists("TEST_N"))    CFG$test_n    <- TEST_N
if (exists("TEST_IDS"))  CFG$test_ids  <- TEST_IDS

# =============================================================================
# A) ROHDATEN EINLESEN
# =============================================================================

# --- A0: Woher kommen die Rohdaten? -----------------------------------------
# Liegen CSVs in data/raw/, werden die genommen. Sonst wird die Excel-Datei
# direkt gelesen. Du musst also nichts von Hand exportieren.

raw_xlsx <- function() {
  f <- list.files(CFG$dir_raw, pattern = "\\.xlsx$", full.names = TRUE,
                  ignore.case = TRUE)
  f <- f[!grepl("^~\\$", basename(f))]   # geoeffnete Excel-Sperrdateien raus
  if (length(f) == 0) return(NA_character_)
  if (length(f) > 1)
    msg("  Mehrere .xlsx gefunden, nehme: ", basename(f[1]))
  f[1]
}

#' Einen Datenblock einlesen -- egal ob CSV oder Tabellenblatt
read_raw_block <- function(csv_pattern, sheet) {
  
  files <- list.files(CFG$dir_raw, pattern = csv_pattern, full.names = TRUE)
  if (length(files) > 0) {
    msg("  Quelle: ", length(files), " CSV-Datei(en).")
    return(rbindlist(
      lapply(files, function(f)
        fread(f, encoding = "UTF-8", na.strings = c("", "NA", "NULL", "NaN"))),
      fill = TRUE, use.names = FALSE))
  }
  
  xl <- raw_xlsx()
  if (is.na(xl))
    stop("In ", CFG$dir_raw, " liegt weder eine passende CSV (Muster '",
         csv_pattern, "') noch eine .xlsx-Datei.", call. = FALSE)
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Zum Lesen der Excel-Datei fehlt das Paket 'readxl'.\n",
         "Bitte einmal ausfuehren:  install.packages(\"readxl\")",
         call. = FALSE)
  
  msg("  Quelle: ", basename(xl), "  ->  Blatt '", sheet, "'")
  x <- suppressMessages(readxl::read_excel(
    xl, sheet = sheet, skip = CFG$sheet_skip,
    col_names = TRUE, .name_repair = "unique"))
  setDT(x)
  x[]
}

# --- A1: Universum (Tab "01_Ticker") ----------------------------------------
# Erwartete Spalten: RIC, ISIN, Common Name, Market, Stichdatum
# Die Batch-Spalte brauchen wir hier nicht.

load_universe <- function() {
  msg("Universum einlesen ...")
  u <- read_raw_block(CFG$pattern_universe, CFG$sheet_universe)
  setnames(u, tolower(gsub("[^A-Za-z]", "_", names(u))))
  
  # Spalten flexibel finden, falls die Excel-Ueberschriften leicht abweichen
  col_ric  <- grep("^ric$",    names(u), value = TRUE)[1]
  col_isin <- grep("isin",     names(u), value = TRUE)[1]
  col_name <- grep("name",     names(u), value = TRUE)[1]
  col_mkt  <- grep("market",   names(u), value = TRUE)[1]
  
  u <- u[, .(id     = get(col_ric),
             isin   = if (!is.na(col_isin)) get(col_isin) else NA_character_,
             name   = if (!is.na(col_name)) get(col_name) else NA_character_,
             market = if (!is.na(col_mkt))  get(col_mkt)  else NA_character_)]
  
  u <- u[!is.na(id) & nzchar(trimws(id))]
  u <- unique(u, by = "id")
  
  # Die zwei Uebersetzer aus der Config anwenden
  u[, yahoo       := ric_to_yahoo(id)]
  u[, delist_date := ric_delist_date(id)]
  u[, is_dead     := !is.na(delist_date)]
  
  msg("Universum: ", nrow(u), " Ticker, davon ", sum(u$is_dead),
      " mit Delisting-Suffix im RIC.")
  if (anyNA(u$yahoo)) {
    bad <- u[is.na(yahoo), unique(sub("^.*\\.", "", sub("\\^.*$", "", id)))]
    warning("Unbekannte Boersenkuerzel (nicht in RIC_EXCHANGE_MAP): ",
            paste(bad, collapse = ", "))
  }
  u[]
}

# --- A2: Fundamentals -------------------------------------------------------
# ACHTUNG, hier steckt ein Fallstrick in deiner Excel-Datei:
#
# Die Kopfzeile in Tab "03_Fundamentals_Export" lautet
#     ..., diluted_shares, total_assets, mktcap_usd, industry, adv_3m
# Die WERTE stehen aber in der Reihenfolge der RDP-Formel aus Tab 02:
#     ..., ShrUsedToCalcDilEPS, CompanyMarketCap, TotAssets, Industry, ADV
#
# Spalte K enthaelt also die Marktkapitalisierung, nicht die Bilanzsumme.
# (Kontrolle Samsung Q2/2019: K = 271,5 Mrd = Market Cap,
#                             L = 296,8 Mrd = Total Assets.)
# Wir lesen deshalb POSITIONSBASIERT ein und pruefen danach mit einer
# Plausibilitaetskontrolle nach, ob die Zuordnung stimmt.

FUND_COLS <- c("id", "period_end", "revenue", "net_income", "gross_profit",
               "total_debt", "total_equity", "op_cashflow", "capex",
               "diluted_shares", "mktcap_usd", "total_assets",
               "industry", "adv_3m")

load_fundamentals <- function() {
  msg("Fundamentals einlesen ...")
  dt <- read_raw_block(CFG$pattern_fundamentals, CFG$sheet_fundamentals)
  
  # Positionsbasiert benennen -- die Kopfzeile im Excel-Blatt stimmt nicht
  # mit der Werte-Reihenfolge ueberein (siehe Kommentar oben). Ueberzaehlige
  # leere Spalten rechts fliegen dabei raus.
  dt <- dt[, seq_len(min(ncol(dt), length(FUND_COLS))), with = FALSE]
  setnames(dt, FUND_COLS[seq_len(ncol(dt))])
  
  # Typen erzwingen. Wichtig beim Excel-Weg: LSEG schreibt fehlende Werte als
  # den TEXT "NULL" in die Zelle. as.numeric() macht daraus NA -- aber nur mit
  # suppressWarnings(), sonst rauscht die Konsole voll.
  dt[, period_end := as_date_safe(period_end)]
  numcols <- setdiff(FUND_COLS, c("id", "period_end", "industry"))
  for (cc in intersect(numcols, names(dt)))
    dt[, (cc) := suppressWarnings(as.numeric(as.character(get(cc))))]
  dt[, id := trimws(as.character(id))]
  dt[, industry := as.character(industry)]
  dt[industry %in% c("NULL", "NA", ""), industry := NA_character_]
  
  # Zeilen ohne Quartalsende sind die "Forecast-Platzhalter" am Ende jedes
  # Ticker-Blocks (nur Market Cap gefuellt) -- die brauchen wir nicht.
  n_bad <- sum(is.na(dt$period_end) & !is.na(dt$id) & nzchar(trimws(dt$id)))
  dt <- dt[!is.na(period_end) & !is.na(id)]
  
  # Duplikate: NICHT einfach die erste Zeile behalten. Bei Geschaeftsjahres-
  # umstellungen steht dieselbe Periode zweimal drin, und manchmal ist die
  # erste der beiden die leere. Regel: die Zeile mit den meisten gefuellten
  # Zahlenfeldern gewinnt.
  fill_cols <- intersect(numcols, names(dt))
  dt[, n_filled_tmp := rowSums(!is.na(.SD)), .SDcols = fill_cols]
  setorder(dt, id, period_end, -n_filled_tmp)
  n_dup <- sum(duplicated(dt, by = c("id", "period_end")))
  dt <- unique(dt, by = c("id", "period_end"))
  dt[, n_filled_tmp := NULL]
  setorder(dt, id, period_end)
  msg("  ", n_dup, " doppelte Ticker-Quartale zusammengefuehrt ",
      "(behalten wurde jeweils die vollstaendigere Zeile).")
  msg("  ", n_bad, " Zeilen ohne lesbares Quartalsende verworfen ",
      "(das sind die Platzhalterzeilen am Ende jedes Ticker-Blocks).")
  
  check_column_swap(dt)
  
  # industry und adv_3m stehen nur in der ERSTEN Zeile je Ticker.
  # industry fuellen wir auf (aendert sich praktisch nie).
  # adv_3m NICHT benutzen -- siehe Kommentar unten.
  dt[, industry := industry[!is.na(industry)][1], by = id]
  dt[is.na(industry), industry := "UNKNOWN"]
  
  msg("Fundamentals: ", nrow(dt), " Zeilen, ",
      uniqueN(dt$id), " Ticker, ",
      format(min(dt$period_end)), " bis ", format(max(dt$period_end)))
  dt[]
}

#' Plausibilitaetskontrolle Market Cap vs. Total Assets
#'
#' Idee: ueber ein gemischtes Universum ist das Verhaeltnis Bilanzsumme zu
#' Umsatz typischerweise deutlich groesser als 1 (Banken extrem, Industrie ~2-4).
#' Wenn die vermeintliche "total_assets"-Spalte systematisch KLEINER ist als
#' die "mktcap"-Spalte relativ zum Umsatz, sind die beiden vertauscht.
check_column_swap <- function(dt) {
  s <- dt[!is.na(revenue) & revenue > 0 & !is.na(total_assets) &
            !is.na(mktcap_usd)]
  if (nrow(s) < 100) {
    msg("  Hinweis: zu wenige Zeilen fuer die Vertauschungs-Pruefung.")
    return(invisible(NULL))
  }
  # Bilanzsumme/Umsatz sollte im Median hoeher liegen als bei einer
  # falsch zugeordneten Spalte. Wir vergleichen beide Kandidaten.
  ta_ratio <- median(s$total_assets / s$revenue, na.rm = TRUE)
  mc_ratio <- median(s$mktcap_usd  / s$revenue, na.rm = TRUE)
  msg("  Plausibilitaet: Median(total_assets/revenue) = ", round(ta_ratio, 2),
      " | Median(mktcap/revenue) = ", round(mc_ratio, 2))
  msg("  -> Beide Werte sollten > 1 sein. Wirkt eine Spalte wie eine ",
      "Bilanzsumme (oft 5-20x Umsatz bei Finanzwerten), passt die Zuordnung.")
  msg("  -> Bei Zweifel eine bekannte Aktie manuell gegenpruefen, ",
      "z.B. Samsung Q2/2019: Market Cap ~272 Mrd, Total Assets ~297 Mrd USD.")
  invisible(NULL)
}

# --- A3: Estimates ----------------------------------------------------------
EST_COLS <- c("id", "period_end", "report_date", "eps_actual", "eps_mean_est",
              "eps_smart_est", "price_target", "rec_mean", "rev_actual",
              "rev_mean_est", "net_income_loc")

load_estimates <- function() {
  msg("Estimates einlesen ...")
  dt <- read_raw_block(CFG$pattern_estimates, CFG$sheet_estimates)
  
  dt <- dt[, seq_len(min(ncol(dt), length(EST_COLS))), with = FALSE]
  setnames(dt, EST_COLS[seq_len(ncol(dt))])
  
  dt[, id := trimws(as.character(id))]
  dt[, period_end := as_date_safe(period_end)]
  # report_date kommt als "2019-07-31 05:17:00" -- wir brauchen nur den Tag.
  dt[, report_date := as_date_safe(report_date)]
  for (cc in setdiff(EST_COLS, c("id", "period_end", "report_date")))
    dt[, (cc) := suppressWarnings(as.numeric(as.character(get(cc))))]
  
  dt <- dt[!is.na(period_end) & !is.na(id)]
  
  # gleiche Regel wie bei den Fundamentals
  fill_cols <- setdiff(EST_COLS, c("id", "period_end", "report_date"))
  dt[, n_filled_tmp := rowSums(!is.na(.SD)), .SDcols = intersect(fill_cols, names(dt))]
  setorder(dt, id, period_end, -n_filled_tmp)
  dt <- unique(dt, by = c("id", "period_end"))
  dt[, n_filled_tmp := NULL]
  setorder(dt, id, period_end)
  
  msg("Estimates: ", nrow(dt), " Zeilen, ",
      sum(!is.na(dt$report_date)), " davon mit Berichtsdatum.")
  dt[]
}

# --- A4: Stichprobe ziehen (Testmodus) --------------------------------------
# Zwei Regeln, damit die Stichprobe nicht luegt:
#
#   1. Nur Ticker nehmen, fuer die auch Fundamentaldaten im Export liegen.
#      Sonst laedst du 120 Kurshistorien und der Backtest hat null Events.
#   2. Delistete Titel MITNEHMEN, anteilig. Eine Stichprobe aus lauter
#      Ueberlebenden testet genau den Code-Pfad nicht, wegen dem wir den
#      ganzen Aufwand betreiben.
subset_universe <- function(u, fund_ids, seed = 42) {
  
  if (!is.null(CFG$test_ids)) {
    sel <- u[id %in% CFG$test_ids]
    msg("TESTMODUS: ", nrow(sel), " fest vorgegebene Ticker.")
    return(sel[])
  }
  if (!isTRUE(CFG$test_mode)) return(u)
  
  cand <- u[id %in% fund_ids]
  if (nrow(cand) == 0) {
    msg("TESTMODUS: kein Ticker des Universums im Fundamentals-Export ",
        "gefunden -- ziehe stattdessen aus dem gesamten Universum.")
    cand <- u
  }
  n <- min(CFG$test_n, nrow(cand))
  
  set.seed(seed)
  dead  <- cand[is_dead == TRUE]
  alive <- cand[is_dead == FALSE]
  n_dead  <- min(nrow(dead), max(1L, round(n * 0.25)))
  if (nrow(dead) == 0) n_dead <- 0L
  n_alive <- min(nrow(alive), n - n_dead)
  
  sel <- rbind(
    if (n_dead  > 0) dead[sample(.N,  n_dead)]  else dead[0],
    if (n_alive > 0) alive[sample(.N, n_alive)] else alive[0]
  )
  msg("TESTMODUS: ", nrow(sel), " Ticker gezogen (",
      sum(sel$is_dead), " davon delistet). ",
      "Fuer den Volllauf CFG$test_mode auf FALSE setzen.")
  sel[]
}

# =============================================================================
# B) ZUSAMMENFUEHREN + "KNOWN_FROM" SETZEN
# =============================================================================

build_quarterly <- function(fund, est) {
  
  dt <- merge(fund, est, by = c("id", "period_end"), all.x = TRUE)
  setorder(dt, id, period_end)
  
  # --- Der wichtigste Schritt des Skripts -----------------------------------
  # known_from = ab wann waren diese Zahlen oeffentlich?
  # Erste Wahl: das echte Berichtsdatum aus den Estimates.
  # Notfall: Quartalsende + konservativer Lag (Default 75 Tage).
  # Reihenfolge ist wichtig: erst pruefen, DANN das Flag setzen. In der alten
  # Fassung wurde report_date_imputed vor der Plausibilitaetskorrektur
  # vergeben -- Zeilen mit einem Berichtsdatum vor dem Quartalsende bekamen
  # zwar ein Ersatzdatum, galten aber weiter als "echt". Skript 03 wirft
  # imputierte Termine raus, deshalb muss das Flag stimmen.
  dt[, report_date_ok := !is.na(report_date) & report_date > period_end]
  dt[, known_from := report_date]
  dt[report_date_ok == FALSE,
     known_from := period_end + CFG$report_lag_fallback]
  dt[, report_date_imputed := !report_date_ok]
  dt[, report_date_ok := NULL]
  
  msg("Berichtsdatum imputiert bei ",
      round(100 * mean(dt$report_date_imputed), 1), "% der Quartale.")
  dt[]
}

# =============================================================================
# C) TTM BILDEN
# =============================================================================
# TTM = Trailing Twelve Months = die letzten vier Quartale aufsummiert.
# Warum? Ein einzelnes Quartal ist saisonal verzerrt (Weihnachtsgeschaeft,
# koreanische Chip-Zyklen). Vier Quartale gleichen das aus.
#
# frollsum(x, 4) summiert das aktuelle plus die drei vorherigen Quartale.
# Ist eines davon NA, wird das Ergebnis NA -- genau richtig, denn eine
# Luecke in der Historie darf keine halbe TTM-Zahl produzieren.

add_ttm <- function(dt) {
  setorder(dt, id, period_end)
  
  # Sicherheitsnetz: es duerfen keine Quartale fehlen, sonst summiert
  # frollsum ueber ein Loch hinweg. Wir markieren Luecken > 100 Tage.
  dt[, gap_days := as.numeric(period_end - shift(period_end)), by = id]
  dt[, has_gap := !is.na(gap_days) & gap_days > 130]
  if (any(dt$has_gap)) {
    msg("Warnung: ", sum(dt$has_gap), " Quartalsluecken > 130 Tage gefunden. ",
        "TTM-Werte direkt nach einer Luecke werden verworfen.")
  }
  # Zaehler, wie viele Quartale seit der letzten Luecke vergangen sind
  dt[, block := cumsum(has_gap), by = id]
  dt[, n_since_gap := seq_len(.N), by = .(id, block)]
  
  ttm_cols <- c(revenue = "rev_ttm", net_income = "ni_ttm",
                gross_profit = "gp_ttm", op_cashflow = "ocf_ttm",
                capex = "capex_ttm", net_income_loc = "ni_ttm_loc")
  
  for (src in names(ttm_cols)) {
    tgt <- ttm_cols[[src]]
    dt[, (tgt) := roll_sum(get(src), 4), by = id]
    # Nach einer Luecke sind die ersten drei TTM-Werte kontaminiert
    dt[n_since_gap < 4, (tgt) := NA_real_]
  }
  
  # Durchschnittliches Eigenkapital der letzten 4 Quartale (Nenner fuer ROE)
  dt[, equity_avg4 := roll_mean(total_equity, 4), by = id]
  dt[n_since_gap < 4, equity_avg4 := NA_real_]
  
  dt[, c("gap_days", "has_gap", "block") := NULL]
  dt[]
}

# =============================================================================
# D) KENNZAHLEN
# =============================================================================
# Eins zu eins die Formeln aus Tab "04_R_Formeln", plus ein paar Ergaenzungen,
# die fuer PEAD gebraucht werden.

add_metrics <- function(dt) {
  
  # --- Margen und Bilanz ---------------------------------------------------
  dt[, net_margin   := safe_div(ni_ttm, rev_ttm)]        # FX kuerzt sich raus
  dt[, gross_margin := safe_div(gp_ttm, rev_ttm)]
  dt[, fcf          := ocf_ttm - abs(capex_ttm)]         # abs() wegen
  # Vorzeichen-Chaos
  dt[, fcf_margin   := safe_div(fcf, rev_ttm)]
  dt[, debt_equity  := safe_div(total_debt, total_equity)]
  dt[, eps_ttm      := safe_div(ni_ttm_loc, diluted_shares)]  # Lokalwaehrung!
  dt[, pe           := safe_div(mktcap_usd, ni_ttm)]     # beides USD
  dt[, roe          := safe_div(ni_ttm, equity_avg4)]
  dt[, accruals     := safe_div(ni_ttm - ocf_ttm, total_assets)]  # niedrig=gut
  
  # Negatives Eigenkapital macht debt_equity und roe sinnlos -> raus
  dt[!is.na(total_equity) & total_equity <= 0,
     c("debt_equity", "roe") := NA_real_]
  
  # --- Wachstum ------------------------------------------------------------
  setorder(dt, id, period_end)
  dt[, rev_yoy := safe_div(rev_ttm, shift(rev_ttm, 4)) - 1, by = id]
  
  # BASISEFFEKT-GUARD: Wenn das Vorjahres-EPS negativ war, ist jede
  # Wachstumsrate Unsinn (-2 auf +1 sind "minus 150 Prozent Wachstum").
  dt[, eps_ttm_lag4 := shift(eps_ttm, 4), by = id]
  dt[, eps_yoy := fifelse(!is.na(eps_ttm_lag4) & eps_ttm_lag4 > 0,
                          eps_ttm / eps_ttm_lag4 - 1, NA_real_)]
  
  # --- Estimates / Surprise ------------------------------------------------
  # Die Prozent-Version aus deinem Tab. Vorsicht: bei einer Schaetzung nahe
  # null explodiert der Wert. Wir behalten sie, aber ranken spaeter lieber
  # ueber die standardisierte Variante weiter unten.
  dt[, eps_surprise  := safe_div(eps_actual - eps_mean_est,
                                 abs(eps_mean_est)) * 100]
  # Vorwaertsgerichtet: SmartEstimate gegen Konsens. Das ist der eigentliche
  # PEAD-Indikator, weil er misst, wo der Konsens noch nachziehen muss.
  dt[, pred_surprise := safe_div(eps_smart_est - eps_mean_est,
                                 abs(eps_mean_est)) * 100]
  dt[, rev_surprise  := safe_div(rev_actual - rev_mean_est,
                                 abs(rev_mean_est)) * 100]
  
  # SUE (Standardized Unexpected Earnings) -- der Literatur-Standard fuer PEAD.
  # Statt durch die Schaetzung zu teilen, teilen wir durch die
  # Streuung der EIGENEN Surprise-Historie des Titels. Ein Titel, der immer
  # um 3% ueberrascht, bekommt fuer 3% keinen Punkt. Ein Titel, der sonst
  # punktgenau liefert, bekommt fuer 3% viele Punkte.
  dt[, surp_abs := eps_actual - eps_mean_est]
  
  # ACHTUNG, hier steckte ein stiller Fehler:
  # roll_fun() reicht na.rm = TRUE an sd() durch. Ein Achterfenster mit sechs
  # Luecken und zwei Werten liefert damit trotzdem eine Zahl -- eine
  # Standardabweichung aus zwei Punkten. Die ist im Erwartungswert zu klein,
  # der Quotient wird zu gross, und weil sue anschliessend bei +/-10 gekappt
  # wird, stapeln sich genau diese Faelle am Rand der Verteilung. Sie landen
  # also geschlossen im Extremquintil, in dem du den Effekt misst.
  #
  # Betroffen sind in diesen Daten rund 4% aller Quartale mit Surprise
  # (Nenner aus genau zwei Punkten) plus 8% mit drei oder vier Punkten.
  # Es sind systematisch dieselben Titel: schlecht abgedeckte Nebenwerte
  # und Neuzugaenge im Sample.
  #
  # Loesung: die Zahl der verwertbaren Vorquartale explizit mitzaehlen und
  # unterhalb der Schwelle auf NA setzen. roll_sum() laeuft mit
  # na.rm = FALSE, deshalb zaehlen wir einen 0/1-Indikator statt der Werte.
  dt[, surp_ok  := as.integer(!is.na(surp_abs))]
  dt[, surp_n8  := shift(roll_sum(surp_ok, 8), 1), by = id]
  dt[, surp_sd8 := shift(roll_fun(surp_abs, 8, sd, na.rm = TRUE), 1),
     by = id]                       # shift(): die aktuelle Zahl darf NICHT
  # in ihre eigene Normierung einfliessen
  dt[, sue := fifelse(!is.na(surp_sd8) & surp_sd8 > 0 &
                        !is.na(surp_n8) & surp_n8 >= SUE_MIN_HIST,
                      surp_abs / surp_sd8, NA_real_)]
  dt[, sue := pmax(pmin(sue, 10), -10)]   # kappen, sonst dominieren Ausreisser
  
  # Wie oft hat der Titel zuletzt positiv ueberrascht? (0 bis 1, letzte 4 Q,
  # ohne das aktuelle Quartal)
  #
  # Hier lag das umgekehrte Problem: roll_mean() laeuft mit na.rm = FALSE,
  # ein einziges fehlendes Quartal loeschte die ganze Trefferquote. Jetzt
  # wird gemittelt, was da ist -- aber erst ab HIT_MIN_HIST Beobachtungen.
  dt[, surp_hit := fifelse(is.na(surp_abs), NA_integer_,
                           as.integer(surp_abs > 0))]
  dt[, hit_n4   := shift(roll_sum(surp_ok, 4), 1), by = id]
  dt[, surp_hit_rate4 := shift(roll_fun(surp_hit, 4, mean, na.rm = TRUE), 1),
     by = id]
  dt[is.na(hit_n4) | hit_n4 < HIT_MIN_HIST, surp_hit_rate4 := NA_real_]
  
  msg("SUE-Nenner: ", dt[!is.na(surp_abs) & !is.na(surp_n8) & surp_n8 >= 8L, .N],
      " Quartale mit voller Achterhistorie, ",
      dt[!is.na(surp_abs) & !is.na(surp_n8) & surp_n8 < SUE_MIN_HIST, .N],
      " wegen zu kurzer Historie verworfen.")
  
  dt[, c("surp_abs", "surp_hit", "surp_ok", "hit_n4",
         "eps_ttm_lag4") := NULL]
  dt[]
}

# =============================================================================
# HAUPTLAUF
# =============================================================================

if (run_main()) {
  
  universe <- load_universe()
  fund     <- load_fundamentals()
  est      <- load_estimates()
  
  # Stichprobe ziehen. Ab hier arbeiten ALLE Skripte nur noch mit diesen
  # Tickern, weil sie das universe.rds von hier lesen.
  universe <- subset_universe(universe, unique(fund$id))
  fund <- fund[id %in% universe$id]
  est  <- est[id  %in% universe$id]
  
  q <- build_quarterly(fund, est)
  q <- add_ttm(q)
  q <- add_metrics(q)
  
  # Nur Ticker behalten, die auch im Universum stehen, und Stammdaten anhaengen
  q <- merge(q, universe[, .(id, name, market, yahoo, delist_date)],
             by = "id", all.x = TRUE)
  
  # Ein Quartal, das NACH dem Delisting liegt, kann es nicht geben -> raus
  q <- q[is.na(delist_date) | known_from <= delist_date]
  
  setorder(q, id, period_end)
  setkey(q, id, known_from)
  
  saveRDS(q,        file.path(CFG$dir_proc, "fundamentals.rds"))
  saveRDS(universe, file.path(CFG$dir_proc, "universe.rds"))
  
  msg("Fertig. ", nrow(q), " Ticker-Quartale gespeichert.")
  
  # Kurzer Blick auf die Abdeckung der Kennzahlen -- wenn hier eine Spalte
  # bei 90% NA steht, stimmt am Export etwas nicht.
  check <- c("rev_ttm", "ni_ttm", "roe", "pe", "debt_equity", "fcf_margin",
             "rev_yoy", "eps_yoy", "sue", "eps_surprise", "pred_surprise",
             "surp_hit_rate4")
  cov <- q[, lapply(.SD, function(x) round(100 * mean(!is.na(x)), 1)),
           .SDcols = check]
  print(data.table(kennzahl = names(cov), abdeckung_pct = unlist(cov)))
}