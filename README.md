# PEAD-Ereignisstudie — Kurzanleitung

**Frage:** Hat eine Gewinnüberraschung im Quartalsbericht Vorhersagekraft für
die nächsten **90 Kalendertage** (Compliance-Haltedauer)?

**Messung:** Einstieg zum Schlusskurs am ersten Handelstag nach dem Bericht,
Ausstieg am ersten Handelstag ≥ 90 Kalendertage später. Gemessen wird die
Rendite über dem gleichgewichteten Heimatmarkt (CAR90).

## Skripte, in dieser Reihenfolge

```
00_config.R             Einstellungen + Hilfsfunktionen (wird von allen geladen)
01_parameter.R          LSEG-Daten einlesen, Berichtsdatum, TTM, SUE & Kennzahlen
02_preise_yahoo.R       Ticker per ISIN zuordnen, Kurse laden (Cache), Reihen prüfen
03_events_car.R         Ereignispanel: je Bericht Signal + CAR90
04_auswertung.R         Kern: CAR-Pfad, Spanne, Long-only, Standardfehler, Kosten
05_diagnose_optional.R  Zusatzdiagnosen (Datenintegrität, fette Ränder, Autokorrelation …)
```

```r
setwd("Pfad/zu/Investing_Backtest")
source("01_parameter.R")
source("02_preise_yahoo.R")
source("03_events_car.R")
source("04_auswertung.R")
```

Jedes Skript speichert sein Ergebnis unter `data/proc/`, Grafiken landen in
`output/auswertung/` bzw. `output/diagnose/`.

## Gegen Lookahead und Survivorship Bias

| Maßnahme | Wo |
|---|---|
| Zahlen gelten erst ab dem echten Berichtsdatum (`known_from`), Ereignisse ohne echtes Datum fliegen raus | 01, 03 |
| Einstieg erst am Handelstag **nach** dem Bericht | `CFG$entry_lag` |
| SUE-Normierung nur mit **vergangenen** Überraschungen | 01 |
| Ränge/Quintile nur gegen Berichte der **letzten 90 Tage** | 03, `rank_trailing()` |
| Universum zum Stichtag 30.06.2019, inkl. später delisteter Titel | Excel `01_Ticker` |
| Verschwindet ein Titel während der Haltedauer, endet die Position zum **letzten verfügbaren Kurs** — das Ereignis bleibt drin | 03 |
| Yahoo-Reihen, die nicht zum RIC passen (falsche Währung, Kürzel neu vergeben), werden verworfen | 02, `output/price_check.csv` |

**Bekannte Lücke:** Yahoo hat für die meisten delisteten Titel keine Kurse.
`02` meldet, wie viele fehlen. Solange sie fehlen, ist das Ergebnis eher zu
optimistisch.

## Ticker-Zuordnung (`data/raw/ticker_map.csv`)

Refinitiv-RICs lassen sich nicht per Regel in Yahoo-Ticker übersetzen
(`BASFn.DE` ist `BAS.DE`). `02` sucht deshalb einmalig je ISIN bei Yahoo und
speichert das Ergebnis in `data/raw/ticker_map.csv`:

| id | isin | yahoo | quelle |
|---|---|---|---|
| BASFn.DE | DE000BASF111 | BAS.DE | isin |

`quelle` ist `isin` (Treffer), `regel` (kein Treffer, Faustregel) oder
`manuell`. Falsche Zuordnungen einfach von Hand korrigieren und `quelle` auf
`manuell` setzen — vorhandene Zeilen werden nie neu gesucht. Danach die
betroffenen Kurse holen: `02` erneut laufen lassen.

Ohne Internet: `CFG$yh_offline <- TRUE` arbeitet nur mit dem Cache.

## Wo du drehen kannst (`CFG` in 00)

| Parameter | Default | Bedeutung |
|---|---|---|
| `hold_days` | 90 | Haltedauer in Kalendertagen |
| `entry_lag` | 1 | Handelstage nach dem Bericht bis zum Einstieg |
| `rank_window_days` | 90 | Vergleichsfenster für Ränge |
| `cost_bps` | 25 | Gebühren je Seite (Spread wird separat geschätzt) |

Jede Änderung ist ein weiterer **Versuch**. Wer viele Varianten testet,
findet zufällig eine, die gut aussieht — führ Buch darüber.

## LSEG-Rohdaten

`01` liest direkt `data/raw/*.xlsx` (Tabs `01_Ticker`,
`03_Fundamentals_Export`, `03_Estimates_Export`) oder — falls vorhanden —
CSV-Exporte mit den Mustern `universe*.csv`, `fundamentals*.csv`,
`estimates*.csv`.

Achtung: In `03_Fundamentals_Export` passt die Kopfzeile nicht zur
Wertereihenfolge (Spalte K = Marktkapitalisierung, L = Bilanzsumme). `01`
liest deshalb positionsbasiert (`FUND_COLS`).
