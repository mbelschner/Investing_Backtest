# =============================================================================
# 00_config.R  --  Zentrale Einstellungen und Hilfsfunktionen
# =============================================================================
#
# WAS PASSIERT HIER?
# Alles, was mehrere Skripte gemeinsam brauchen, steht an EINER Stelle:
# Pfade, Pakete, Parameter der Ereignisstudie und zwei kleine "Uebersetzer":
#   - RIC  -> Yahoo-Ticker   (Faustregel; 02 ersetzt sie per ISIN-Suche)
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
#   Investing_Backtest/
#     data/raw/      <- LSEG-Excel (oder CSV-Exporte) + ticker_map.csv
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

# -----------------------------------------------------------------------------
# 3. Parameter der Ereignisstudie
# -----------------------------------------------------------------------------
# Haltedauer: Compliance verlangt mindestens 90 Tage. Gemessen wird deshalb
# genau so, wie du handeln wuerdest: Einstieg zum Schlusskurs an Tag
# entry_lag nach dem Bericht, Ausstieg am ersten Handelstag, der mindestens
# hold_days KALENDERtage spaeter liegt.
CFG$hold_days <- 90     # Kalendertage
CFG$entry_lag <- 1      # Handelstage NACH dem Bericht bis zum Einstieg.
                        # 0 waere Lookahead: der Bericht kommt oft nach
                        # Handelsschluss oder vorboerslich.

# Raenge (SUE-Quintil, Liquiditaetsdezil) werden NUR gegen Berichte gebildet,
# die zum Einstieg schon bekannt waren: die der letzten rank_window_days Tage.
CFG$rank_window_days <- 90
CFG$rank_min_n       <- 50   # weniger Vergleichsberichte -> kein Rang

# Gebuehren pro Seite in Basispunkten (der Spread wird separat geschaetzt).
CFG$cost_bps <- 25

# Wenn kein report_date vorliegt: konservativer Melde-Lag in Kalendertagen.
CFG$report_lag_fallback <- 75

# -----------------------------------------------------------------------------
# TESTMODUS
# -----------------------------------------------------------------------------
# Fuer den ersten Durchlauf: nur eine Handvoll Ticker statt aller 2.365.
# Der Preis-Cache ist pro Ticker, also ist nichts verloren -- was du im Test
# heruntergeladen hast, wird beim spaeteren Volllauf wiederverwendet.
CFG$test_mode <- FALSE   # TRUE = nur eine Stichprobe rechnen
CFG$test_n    <- 120     # wie viele Ticker
CFG$test_ids  <- NULL    # oder konkrete RICs, z.B. c("005930.KS", "2330.TW")

# -----------------------------------------------------------------------------
# 4. Yahoo-Downloader
# -----------------------------------------------------------------------------
CFG$yh_sleep        <- 0.35  # Sekunden Pause zwischen zwei Requests
CFG$yh_max_retry    <- 4     # Wiederholungen bei Fehler
# TRUE = nichts herunterladen, nur aus dem vorhandenen Cache arbeiten.
CFG$yh_offline      <- FALSE
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
#
# Verlaesslich ist nur die ENDUNG. Die Wurzel weicht vor allem in DE/FR/AT ab
# (BASFn.DE ist bei Yahoo BAS.DE, SAPG.DE ist SAP.DE). Deshalb sucht 02 den
# Yahoo-Ticker per ISIN und nimmt diese Regel nur als Rueckfallebene.
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

# Erwartete Handelswaehrung je Markt. Dient als Plausibilitaetscheck: liefert
# Yahoo fuer eine britische Aktie eine Euro-Reihe, ist es ein anderes Papier
# (so geschehen bei BAES.L). GBX = britische Pence, so notiert die LSE.
MARKET_CCY <- c(
  "United States" = "USD", "Hong Kong" = "HKD", "Japan" = "JPY",
  "Korea" = "KRW", "Taiwan" = "TWD", "United Kingdom" = "GBX",
  "Switzerland" = "CHF", "Sweden" = "SEK", "Denmark" = "DKK",
  "Norway" = "NOK", "Poland" = "PLN",
  "Germany" = "EUR", "France" = "EUR", "Austria" = "EUR", "Italy" = "EUR",
  "Netherlands" = "EUR", "Spain" = "EUR", "Finland" = "EUR",
  "Belgium" = "EUR", "Ireland" = "EUR", "Portugal" = "EUR"
)

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

msg("Config geladen. Daten von ", format(CFG$date_start), " bis ",
    format(CFG$date_end), ", Haltedauer ", CFG$hold_days, " Tage.")