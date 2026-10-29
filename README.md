# PEAD-Pipeline — Kurzanleitung

Vier Skripte, in dieser Reihenfolge auszuführen. Jedes speichert sein Ergebnis
als `.rds`, damit du bei einem Fehler nicht alles neu rechnen musst.

```
00_config.R          Einstellungen + Hilfsfunktionen (wird von allen geladen)
01_parameter.R       LSEG-Exporte einlesen, TTM, alle Kennzahlen
02_preise_yahoo.R    Kursdaten holen (separat, mit Cache)
03_universum_pit.R   Point-in-Time-Panel ohne Survivorship Bias
04_backtest_pead.R   Backtest, 4 Varianten, DSR, Permutationstest
```

## Ordner anlegen

```
pead/
  00_config.R … 04_backtest_pead.R
  data/raw/        <- hier die CSVs aus Excel ablegen
  data/cache/      <- legt Skript 02 selbst an
  data/proc/
  output/
```

Arbeitsverzeichnis in R auf `pead/` setzen (`setwd()`), dann:

```r
source("01_parameter.R")
source("02_preise_yahoo.R")
source("03_universum_pit.R")
source("04_backtest_pead.R")
```

## CSV-Export aus dem Excel-File

Drei Dateien nach `data/raw/`:

| Datei | Quelle | Wichtig |
|---|---|---|
| `universe.csv` | Tab `01_Ticker` | Export **ab Zeile 4** (Kopfzeile `RIC | ISIN | Common Name | Market | Stichdatum`). Die Titelzeilen 1–2 müssen weg. |
| `fundamentals_01.csv`, `_02.csv`, … | Tab `03_Fundamentals_Export` | Export **ab Zeile 4**. Mehrere Batches einfach als mehrere Dateien ablegen — Skript 01 sammelt alle mit dem Muster `fundamentals*.csv` ein. |
| `estimates_01.csv`, … | Tab `03_Estimates_Export` | Analog, Muster `estimates*.csv`. |

Speichern als **CSV UTF-8 (durch Trennzeichen getrennt)**. `fread()` erkennt
Komma und Semikolon automatisch.

### Achtung: Spaltenreihenfolge Fundamentals

Skript 01 liest die Fundamentals **positionsbasiert** ein, weil die Kopfzeile in
`03_Fundamentals_Export` nicht zur Werte-Reihenfolge passt:

```
Kopfzeile sagt:   … diluted_shares, total_assets, mktcap_usd, industry, adv_3m
Werte kommen als: … diluted_shares, mktcap_usd,   total_assets, industry, adv_3m
```

Spalte K ist die Marktkapitalisierung, Spalte L die Bilanzsumme. Wenn du die
Excel-Kopfzeile irgendwann korrigierst, musst du `FUND_COLS` in Skript 01
mit anpassen. Beim Einlesen läuft eine Plausibilitätsprüfung mit, die die
Medianwerte in die Konsole schreibt.

## Wo du drehen willst

Alles in `CFG` (Skript 00):

| Parameter | Default | Bedeutung |
|---|---|---|
| `hold_days` | 90 | Haltedauer in Kalendertagen |
| `entry_lag` | 1 | Handelstage nach dem Bericht bis zum Einstieg |
| `min_mktcap` / `min_adv_usd` | 1e9 / 5e6 | Hygienefilter |
| `sector_cap` | 0.30 | max. Anteil je TRBC-Industry |
| `cost_bps` | 25 | Transaktionskosten je Seite |
| `delist_return` | −0.30 | Abschlag bei erzwungenem Ausstieg |
| `n_trials` | 4 | **ehrliche** Zahl der Versuche für die DSR |

## Laufzeiten (Richtwerte, 2.365 Ticker)

| Schritt | Erster Lauf | Danach |
|---|---|---|
| 01 Parameter | < 1 min | < 1 min |
| 02 Preise | 15–20 min | 2–5 min (Cache) |
| 03 Panel | 3–5 min | 3–5 min |
| 04 Backtest | 2 min + Permutationstest ~10 min | dito |
