# =============================================================================
# 00_config.R  --  Zentrale Einstellungen und Hilfsfunktionen
# =============================================================================
#
# WAS PASSIERT HIER?
# Alles, was mehrere Skripte gemeinsam brauchen, steht an EINER Stelle:
# Pfade, Pakete, Strategie-Parameter und zwei kleine "Uebersetzer":
#   - RIC  -> Yahoo-Ticker   (005930.KS -> 005930.KS, BRKb.N -> BRK-B, ...)
#   - RIC  -> Delisting-Datum (.N^J22 -> 31.10.2022)
#
# Diese Datei wird von allen anderen Skripten oben mit source() geladen.
# Sie rechnet selbst nichts aus.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(httr2)
  library(jsonlite)
})

setDTthreads(0)  # 0 = alle verfuegbaren Kerne nutzen

# -----------------------------------------------------------------------------
# 1. Pfade
# -----------------------------------------------------------------------------
# Erwartete Ordnerstruktur:
#   pead/
#     data/raw/      <- hier die CSV-Exporte aus Excel ablegen
#     data/cache/    <- Yahoo-Preise, ein .rds je Ticker (wird angelegt)
#     data/proc/     <- fertig gerechnete Panels (wird angelegt)
#     output/        <- Backtest-Ergebnisse (wird angelegt)

CFG <- list()

CFG$dir_root  <- normalizePath(".", mustWork = FALSE)
CFG$dir_raw   <- file.path(CFG$dir_root, "data", "raw")
CFG$dir_cache <- file.path(CFG$dir_root, "data", "cache")
CFG$dir_proc  <- file.path(CFG$dir_root, "data", "proc")
CFG$dir_out   <- file.path(CFG$dir_root, "output")

for (d in c(CFG$dir_raw, CFG$dir_cache, CFG$dir_proc, CFG$dir_out)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# Rohdaten koennen auf ZWEI Wegen kommen:
#
#   a) Direkt als .xlsx in data/raw/ -- dann werden die Tabellenblaetter
#      unten gelesen. Bequem, aber langsam bei grossen Dateien.
#   b) Als CSV-Exporte in data/raw/ -- werden bevorzugt, sobald welche da
#      sind. Muster, damit mehrere Batch-Exporte (Batch 1..40) automatisch
#      eingesammelt und untereinander gehaengt werden.
CFG$pattern_fundamentals <- "^fundamentals.*\\.csv$"
CFG$pattern_estimates    <- "^estimates.*\\.csv$"
CFG$pattern_universe     <- "^universe.*\\.csv$"

# Namen der Tabellenblaetter in der Excel-Datei
CFG$sheet_universe     <- "01_Ticker"
CFG$sheet_fundamentals <- "03_Fundamentals_Export"
CFG$sheet_estimates    <- "03_Estimates_Export"

# In allen drei Blaettern stehen oben zwei Titelzeilen plus eine Leerzeile,
# die echte Kopfzeile ist Zeile 4. Also drei Zeilen ueberspringen.
CFG$sheet_skip <- 3

# -----------------------------------------------------------------------------
# 2. Zeitraum
# -----------------------------------------------------------------------------
CFG$date_start <- as.Date("2019-07-01")   # Beginn der LSEG-Historie
CFG$date_end   <- Sys.Date()

# Der Backtest startet spaeter als die Daten, weil wir Vorlauf brauchen:
# 4 Quartale fuer TTM + 252 Tage fuer 12M-Momentum.
CFG$date_bt_start <- as.Date("2021-01-01")

# -----------------------------------------------------------------------------
# 3. Strategie-Parameter
# -----------------------------------------------------------------------------
# Haltedauer: du willst mindestens 90 Tage halten. PEAD ist genau darauf
# ausgelegt -- der Drift laeuft klassisch ueber ein Quartal nach dem Bericht.
CFG$hold_days     <- 90     # Kalendertage
CFG$entry_lag     <- 1      # Handelstage NACH dem Report-Datum einsteigen.
# 0 waere Lookahead: der Bericht kommt oft nach
# Handelsschluss oder vorboerslich.
CFG$rebal_weekday <- 1      # 1 = Montag. Events werden wochenweise gebuendelt.

# Hygiene
CFG$min_mktcap    <- 1e9    # 1 Mrd USD
CFG$min_adv_usd   <- 5e6    # 5 Mio USD Tagesumsatz, rollierend selbst gerechnet
CFG$max_growth    <- 3.0    # Wachstum ueber +300% = Sondereffekt, raus
CFG$sector_cap    <- 0.30   # max. 30% des Portfolios je TRBC-Industry
CFG$max_stale_days<- 200    # Fundamentaldaten aelter als 200 Tage = unbrauchbar

# Kosten (pro Seite, in Basispunkten). Dein Universum ist gemischt:
# US/DE guenstig, KR/TW/HK teurer (Stempelsteuer, Spread).
CFG$cost_bps      <- 25

# Delisting-Rendite: was passiert mit einer offenen Position, wenn der Titel
# verschwindet? Shumway (1997) findet fuer NYSE/AMEX rund -30%, fuer Nasdaq
# rund -55%. Wir nehmen -30% als Default und testen die Sensitivitaet.
CFG$delist_return <- -0.30

# Wenn kein report_date vorliegt: konservativer Melde-Lag in Kalendertagen.
CFG$report_lag_fallback <- 75

# -----------------------------------------------------------------------------
# TESTMODUS
# -----------------------------------------------------------------------------
# Fuer den ersten Durchlauf: nur eine Handvoll Ticker statt aller 2.365.
# Der Preis-Cache ist pro Ticker, also ist nichts verloren -- was du im Test
# heruntergeladen hast, wird beim spaeteren Volllauf wiederverwendet.
#
# Diese drei Werte werden von run_all.R ueberschrieben, falls du sie dort
# setzt. Wenn du ein Skript einzeln laufen laesst, gelten die Werte hier.
CFG$test_mode <- FALSE   # TRUE = nur eine Stichprobe rechnen
CFG$test_n    <- 120     # wie viele Ticker
CFG$test_ids  <- NULL    # oder konkrete RICs, z.B. c("005930.KS", "2330.TW")

# Ehrliche Anzahl der Versuche fuer die Deflated Sharpe Ratio.
# Vier Varianten sind der Startwert. Jede weitere Parameterrunde, jeder
# verworfene Filter, jedes "probier mal 60 statt 90 Tage" erhoeht diese Zahl.
# Fuehr Buch darueber und trag den echten Wert ein -- sonst rechnest du dir
# die Signifikanz schoen.
CFG$n_trials <- 4

# -----------------------------------------------------------------------------
# 4. Yahoo-Downloader
# -----------------------------------------------------------------------------
CFG$yh_sleep        <- 0.35  # Sekunden Pause zwischen zwei Requests
CFG$yh_max_retry    <- 4     # Wiederholungen bei Fehler
# Wie alt darf der Cache eines LEBENDEN Titels werden, bevor die Historie
# komplett neu geladen wird? Komplett deshalb, weil adjclose bei jeder
# Dividende rueckwirkend fuer die ganze Vergangenheit neu skaliert wird --
# ein blosses Anhaengen neuer Tage wuerde einen Bruch in der Renditereihe
# erzeugen. Bei taeglichem Neustart wird also nichts geladen, bei woechent-
# lichem die lebenden Titel.
CFG$yh_refresh_days <- 7

# Und bei einem TOTEN Titel? Nie. Wenn ein Papier 2022 delistet wurde, aendert
# sich an seiner Historie nichts mehr. Diese rund 200 Ticker jedes Mal neu zu
# ziehen ist reine Wartezeit. Zum Erzwingen: fetch_prices(..., force = TRUE).
CFG$yh_dead_refresh_days <- 3650
CFG$yh_user_agent   <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

# -----------------------------------------------------------------------------
# 5. Uebersetzer: RIC -> Yahoo-Ticker
# -----------------------------------------------------------------------------
# Refinitiv und Yahoo benutzen unterschiedliche Boersenkuerzel.
# Diese Tabelle uebersetzt die Endung. "" bedeutet: Yahoo braucht keine Endung
# (gilt fuer alle US-Boersen).
RIC_EXCHANGE_MAP <- c(
  # USA -- Yahoo hat keinen Boersen-Suffix
  "N" = "", "OQ" = "", "O" = "", "A" = "", "P" = "", "Z" = "", "K" = "",
  # Asien
  "HK" = ".HK", "T" = ".T", "KS" = ".KS", "KQ" = ".KQ",
  "TW" = ".TW", "TWO" = ".TWO",
  # Europa
  "DE" = ".DE", "F" = ".F", "L" = ".L", "PA" = ".PA",
  "S"  = ".SW",   # SIX Swiss: Refinitiv ".S" -> Yahoo ".SW"
  "ST" = ".ST", "VI" = ".VI", "MI" = ".MI", "AS" = ".AS",
  "MC" = ".MC", "CO" = ".CO", "HE" = ".HE", "OL" = ".OL",
  "BR" = ".BR", "WA" = ".WA", "LS" = ".LS",
  "I"  = ".IR"    # Euronext Dublin: Refinitiv ".I" -> Yahoo ".IR"
)

#' RIC in einen Yahoo-Ticker uebersetzen
#'
#' Beispiele:
#'   "005930.KS"  -> "005930.KS"
#'   "AAPL.OQ"    -> "AAPL"
#'   "BRKb.N"     -> "BRK-B"    (Aktiengattung: kleiner Buchstabe -> "-X")
#'   "NESN.S"     -> "NESN.SW"
#'   "ABC.N^J22"  -> "ABC"      (Delisting-Suffix wird abgeschnitten)
ric_to_yahoo <- function(ric) {
  ric <- trimws(as.character(ric))
  
  # Delisting-Suffix "^J22" entfernen -- der interessiert Yahoo nicht
  base <- sub("\\^[A-La-l][0-9]{2}$", "", ric)
  
  # In Wurzel und Boersenkuerzel zerlegen (am LETZTEN Punkt trennen)
  has_dot <- grepl("\\.", base)
  root <- ifelse(has_dot, sub("\\.[^.]*$", "", base), base)
  exch <- ifelse(has_dot, sub("^.*\\.", "", base), NA_character_)
  
  # Aktiengattung: In RICs steht die Gattung als KLEINER Buchstabe am Ende
  # der Wurzel (BRKb = Berkshire B). Yahoo schreibt BRK-B.
  is_class <- grepl("[a-z]$", root) & grepl("^[A-Z]+[a-z]$", root)
  root <- ifelse(is_class,
                 paste0(substr(root, 1, nchar(root) - 1), "-",
                        toupper(substring(root, nchar(root)))),
                 root)
  
  suffix <- unname(RIC_EXCHANGE_MAP[toupper(exch)])
  suffix[is.na(suffix)] <- NA_character_   # unbekannte Boerse -> NA
  
  out <- ifelse(is.na(exch), root,
                ifelse(is.na(suffix), NA_character_, paste0(root, suffix)))
  out
}

# -----------------------------------------------------------------------------
# 6. Uebersetzer: RIC -> Delisting-Datum
# -----------------------------------------------------------------------------
# Refinitiv haengt an tote Instrumente "^" + Monatsbuchstabe + Jahr an.
# A=Jan, B=Feb, C=Mrz, D=Apr, E=Mai, F=Jun, G=Jul, H=Aug, I=Sep, J=Okt,
# K=Nov, L=Dez.  ".N^J22" = NYSE, Delisting Oktober 2022.
#
# Das ist unser wichtigstes Werkzeug gegen Survivorship Bias: wir wissen
# taggenau, ab wann ein Titel NICHT mehr investierbar war.
ric_delist_date <- function(ric) {
  ric <- as.character(ric)
  m <- regmatches(ric, regexpr("\\^[A-La-l][0-9]{2}$", ric))
  out <- as.Date(rep(NA_character_, length(ric)))
  hit <- regexpr("\\^[A-La-l][0-9]{2}$", ric) > 0
  if (!any(hit)) return(out)
  
  code  <- substring(m, 2)                      # z.B. "J22"
  mon   <- match(toupper(substr(code, 1, 1)), LETTERS[1:12])
  yr    <- 2000L + as.integer(substr(code, 2, 3))
  
  # Wir setzen das Delisting auf den LETZTEN Tag des Monats.
  # Trick: erster Tag des Folgemonats minus 1 Tag.
  nxt_m <- ifelse(mon == 12L, 1L, mon + 1L)
  nxt_y <- ifelse(mon == 12L, yr + 1L, yr)
  out[hit] <- as.Date(sprintf("%04d-%02d-01", nxt_y, nxt_m)) - 1L
  out
}

# -----------------------------------------------------------------------------
# 7. Kleine Helfer
# -----------------------------------------------------------------------------

# Sicheres "wenn NULL/leer, dann Ersatzwert"
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

#' Robuster Datums-Parser
#'
#' Warum das noetig ist: Sobald in einer Excel-Spalte neben Datumswerten auch
#' der Text "NULL" steht (und genau das schreibt LSEG bei fehlenden Werten),
#' liest readxl die GANZE Spalte als Text. Die Datumswerte kommen dann als
#' Excel-Seriennummern an -- "43646" statt "2019-06-30". as.Date() steigt da
#' mit "Zeichenkette ist nicht in einem eindeutigen Standardformat" aus.
#'
#' Diese Funktion nimmt alles entgegen, was in der Praxis auftaucht:
#'   - echte Date- und POSIXct-Objekte      (CSV-Weg, sauber getypte Spalten)
#'   - Excel-Seriennummern als Zahl oder Text ("43646")
#'   - "2019-06-30" und "2019-07-31 05:17:00"
#'   - "30.06.2019" und "06/30/2019"
#'   - "20190630"
#'   - "NULL", "NA", "" und andere Leerwerte  -> NA
as_date_safe <- function(x) {
  
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  
  # Zahlen: Excel zaehlt Tage ab dem 30.12.1899 (der beruehmte Schaltjahr-Bug)
  if (is.numeric(x)) {
    out <- as.Date(rep(NA_character_, length(x)))
    ok <- !is.na(x) & x > 10000 & x < 80000
    out[ok] <- as.Date(x[ok], origin = "1899-12-30")
    return(out)
  }
  
  s <- trimws(as.character(x))
  s[s %in% c("", "NULL", "NA", "NaN", "#N/A", "-")] <- NA_character_
  out <- as.Date(rep(NA_character_, length(s)))
  
  # a) reine Ziffernfolgen
  is_num <- !is.na(s) & grepl("^[0-9]+(\\.[0-9]+)?$", s)
  
  #    a1) achtstellig wie "20190630" -> Jahr-Monat-Tag
  ymd <- is_num & nchar(s) == 8L & grepl("^(19|20)", s)
  if (any(ymd))
    out[ymd] <- suppressWarnings(as.Date(s[ymd], format = "%Y%m%d"))
  
  #    a2) alles andere Numerische -> Excel-Seriennummer
  ser <- is_num & !ymd
  if (any(ser)) {
    v <- suppressWarnings(as.numeric(s[ser]))
    d <- as.Date(rep(NA_character_, length(v)))
    okv <- !is.na(v) & v > 10000 & v < 80000
    d[okv] <- as.Date(v[okv], origin = "1899-12-30")
    out[ser] <- d
  }
  
  # b) Textformate. Der Reihe nach durchprobieren, bis eines greift.
  rest <- which(!is.na(s) & !is_num)
  if (length(rest)) {
    head10 <- substr(s[rest], 1, 10)
    d <- as.Date(rep(NA_character_, length(rest)))
    for (fmt in c("%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y", "%m/%d/%Y", "%d/%m/%Y")) {
      miss <- is.na(d)
      if (!any(miss)) break
      d[miss] <- suppressWarnings(as.Date(head10[miss], format = fmt))
    }
    out[rest] <- d
  }
  out
}

# Division, die bei Nenner 0 oder NA sauber NA liefert statt Inf
safe_div <- function(a, b) {
  b2 <- ifelse(is.na(b) | b == 0, NA_real_, b)
  a / b2
}

# Winsorisieren: Extremwerte auf Quantile kappen. Verhindert, dass ein
# einzelner Ausreisser eine Rangliste oder Regression dominiert.
winsorize <- function(x, p = 0.01) {
  q <- quantile(x, c(p, 1 - p), na.rm = TRUE)
  pmin(pmax(x, q[1]), q[2])
}

# Cross-sectional z-Score (robust: winsorisiert vor der Standardisierung)
zscore <- function(x, p = 0.01) {
  x <- winsorize(x, p)
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
}

# Perzentilrang von 0 bis 1. Kleiner Wert = niedriger Rang.
# frank() aus data.table behandelt NA korrekt (na.last = "keep").
prank <- function(x) {
  n <- sum(!is.na(x))
  if (n == 0L) return(rep(NA_real_, length(x)))
  data.table::frank(x, na.last = "keep", ties.method = "average") / n
}

# Naechster Handelstag >= d aus einem Kalendervektor
next_trading_day <- function(d, calendar) {
  # findInterval braucht echte Zahlen, deshalb as.numeric() auf beiden Seiten.
  idx <- findInterval(as.numeric(d) - 1e-9, as.numeric(calendar)) + 1L
  idx[idx < 1L | idx > length(calendar)] <- NA_integer_
  calendar[idx]
}

# Rollierende Fensterfunktionen brechen ab, wenn die Reihe kuerzer ist als das
# Fenster. Diese Wrapper geben in dem Fall sauber NA zurueck.
roll_sum <- function(x, n) {
  if (length(x) < n) return(rep(NA_real_, length(x)))
  data.table::frollsum(x, n, algo = "exact")
}
roll_mean <- function(x, n) {
  if (length(x) < n) return(rep(NA_real_, length(x)))
  data.table::frollmean(x, n, algo = "exact")
}
roll_fun <- function(x, n, FUN, ...) {
  if (length(x) < n) return(rep(NA_real_, length(x)))
  data.table::frollapply(x, n, FUN, ...)
}

# Einheitlicher Schalter: wenn ein Skript nur wegen seiner Funktionen geladen
# wird (source()), soll sein Hauptlauf NICHT starten.
run_main <- function() !isTRUE(getOption("pead.lib_only"))

msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

msg("Config geladen. Backtest-Fenster: ",
    format(CFG$date_bt_start), " bis ", format(CFG$date_end))