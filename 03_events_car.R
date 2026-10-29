# =============================================================================
# 03_events_car.R  --  Ereignis-Panel bauen (Quartalsbericht x Kursreaktion)
# =============================================================================
#
# DIE FRAGE, die dieses Skript stellt:
#
#   Was macht der Kurs in den 90 Kalendertagen nach einem Quartalsbericht,
#   in Abhaengigkeit davon, wie stark der Bericht ueberrascht hat?
#
# Gemessen wird wie du handeln wuerdest: Einstieg zum Schlusskurs an Tag
# CFG$entry_lag nach dem Bericht, Ausstieg am ersten Handelstag, der
# mindestens CFG$hold_days Kalendertage spaeter liegt (Compliance: 90 Tage).
#
# Kein Portfolio, keine Gewichtung, keine Filter. Erst wenn dieser
# Zusammenhang in den Rohdaten sichtbar ist, lohnt sich ein Backtest.
#
# DIE DATENSTRUKTUR, die dabei entsteht:
#
#   events.rds      1 Zeile je (Ticker x Quartalsbericht)
#                   Signal (sue, surprise) + Reaktion (car_ann, car90)
#                   + Kontext (Liquiditaet, Spread, Marktsegment)
#
#   event_time.rds  1 Zeile je (Ereignis x Ereignistag)
#                   Ereignistag k laeuft von -10 bis +80. Das ist die
#                   Grundlage fuer die Pfad-Grafiken.
#
#   px_daily.rds    Tageskurse mit Rendite, Marktrendite, Abnormalrendite
#
# EREIGNISZEIT -- der zentrale Begriff:
# Statt Kalenderdaten zaehlen wir Handelstage relativ zum Bericht. Tag 0 ist
# der erste Handelstag am oder nach dem Berichtstermin, Tag +1 der naechste,
# Tag -1 der davor. So liegen alle Ereignisse uebereinander, obwohl sie an
# verschiedenen Kalendertagen stattfanden.
# =============================================================================

source("00_config.R")

# -----------------------------------------------------------------------------
# Parameter der Ereignisstudie
# -----------------------------------------------------------------------------
# Diese Fenster sind die Konvention aus der PEAD-Literatur. Wenn du sie
# aenderst, ist das ein weiterer "Versuch" im Sinne der Multiple-Testing-
# Korrektur -- also nicht heimlich schrauben, bis es passt.

EV <- list(
  pre        = -60L,        # fruehester Ereignistag, den wir laden
  post       =  80L,        # spaetester Ereignistag (90 Kalendertage sind
                            # 60-65 Handelstage, 80 laesst Luft fuer Feiertage)
  win_ann    = c(-1L, 1L),  # Ankuendigungsfenster: Reaktion AM Bericht
  win_vol    = c(-60L, -11L), # Vorlauf fuer die Volatilitaetsnormierung
  win_run    = c(-10L, -2L),  # Vorlauf-Fenster ("run-up" vor dem Bericht)
  path_from  = -10L         # ab hier wird der CAR-Pfad kumuliert
)
# Das Driftfenster ist kein fester Bereich mehr, sondern je Ereignis:
# von Tag entry_lag + 1 bis zum Ausstiegstag (siehe Abschnitt 5).

# Warum -1 bis +1 und nicht nur Tag 0?
# Wir wissen aus dem Export nicht sicher, ob der Bericht vor Handelsbeginn,
# waehrend des Tages oder nach Handelsschluss kam. Ein Dreitagesfenster faengt
# beide Faelle ein. Preis dafuer: das Fenster enthaelt einen Tag, an dem die
# Nachricht noch nicht draussen war -- deshalb beginnt das Drift-Fenster erst
# nach dem Einstieg an Tag +1, also bei +2. Ueberlappen sich Reaktions- und Driftfenster, misst
# du die Reaktion zweimal und nennst das Drift.

# =============================================================================
# 1. EINLESEN
# =============================================================================

universe <- readRDS(file.path(CFG$dir_proc, "universe.rds"))
fund     <- readRDS(file.path(CFG$dir_proc, "fundamentals.rds"))
px       <- readRDS(file.path(CFG$dir_proc, "prices.rds"))
fx_file  <- file.path(CFG$dir_proc, "fx.rds")
fx       <- if (file.exists(fx_file)) readRDS(fx_file) else NULL

setDT(px); setDT(fund)
px[, currency := toupper(currency)]
px[is.na(currency), currency := "USD"]

# =============================================================================
# 2. TAGESRENDITEN UND MARKTBEREINIGUNG
# =============================================================================

setorder(px, id, date)
px <- merge(px, universe[, .(id, market)], by = "id", all.x = TRUE)
px[is.na(market), market := "UNKNOWN"]

# Rendite aus dem BEREINIGTEN Kurs -- Dividenden und Splits sind darin drin.
px[, ret := adjclose / shift(adjclose) - 1, by = id]

# Ein Tagessprung ueber +100% oder unter -80% ist fast immer ein Datenfehler
# (nicht angepasster Split). Wir setzen ihn auf NA statt ihn mitzurechnen.
# Achtung: das ist eine ANNAHME. In der Diagnose zaehlen wir, wie oft das
# passiert -- wenn es haeufig ist, hast du ein Datenproblem, keinen Ausreisser.
px[, ret_raw := ret]
px[!is.na(ret) & (ret > 1 | ret < -0.8), ret := NA_real_]

# Zweiter Fehlertyp: der BEREINIGTE Kurs springt, der echte nicht. Dann hat
# Yahoo eine Dividende oder einen Split falsch eingerechnet (ORRON.ST springt
# so jedes Quartal um 70-130%). Echte Splits sehen umgekehrt aus: der echte
# Kurs springt, der bereinigte nicht -- die bleiben unberuehrt.
px[, ret_close := close / shift(close) - 1, by = id]
px[!is.na(ret) & abs(ret) > 0.25 & abs(ret_close) < 0.05, ret := NA_real_]
px[, ret_close := NULL]

# --- Marktindex je Land ------------------------------------------------------
# Warum je Land und nicht global? Dein Universum reicht von Taiwan bis Texas.
# Ein globaler Index bereinigt einen koreanischen Titel um US-Bewegungen, die
# er nie mitgemacht hat. Der Index ist gleichgewichtet -- das ist eine
# Vereinfachung des Marktmodells mit Beta = 1.
#
# ANNAHME, die du kennen solltest: Beta = 1 fuer alle. Ein Titel mit Beta 1,5
# bekommt in einem starken Markt eine kuenstlich positive Abnormalrendite.
# Ueber viele Ereignisse mittelt sich das heraus, SOLANGE Berichtstermine
# nicht mit Marktphasen korrelieren. Sie tun es aber ein bisschen (alle
# berichten in derselben Woche) -- genau deshalb rechnen wir in der Diagnose
# spaeter geclusterte Standardfehler je Quartal.
mkt <- px[!is.na(ret), .(mkt_ret = mean(ret), n_mkt = .N), by = .(market, date)]
glb <- px[!is.na(ret), .(glob_ret = mean(ret)), by = date]
px <- merge(px, mkt, by = c("market", "date"), all.x = TRUE)
px <- merge(px, glb, by = "date", all.x = TRUE)

# Bei duennen Maerkten (Polen: 9 Titel) waere der eigene Titel ein Zehntel des
# "Marktes" -- dann bereinigst du ihn gegen sich selbst. Ab unter 10 Titeln
# nehmen wir deshalb den globalen Index.
px[is.na(mkt_ret) | n_mkt < 10L, mkt_ret := glob_ret]
px[, ar := ret - mkt_ret]          # abnormal return

# --- Liquiditaet in USD ------------------------------------------------------
if (!is.null(fx) && nrow(fx)) {
  setkey(fx, currency, date)
  setorder(px, currency, date)
  px <- fx[px, on = .(currency, date), roll = TRUE]   # Feiertage ueberbruecken
}
if (!"fx_usd" %in% names(px)) px[, fx_usd := 1]
px[is.na(fx_usd), fx_usd := 1]

setorder(px, id, date)
px[, turnover_usd := volume * close * fx_usd]
px[, adv63_usd := frollmean(turnover_usd, 63, na.rm = TRUE), by = id]

# --- Spread-Schaetzer nach Corwin/Schultz (2012) -----------------------------
# Wir haben keine Geld-Brief-Spannen, nur Tagesdaten. Corwin und Schultz
# schaetzen den Spread aus dem Verhaeltnis von Tagesspanne (Hoch/Tief) zu
# Zweitagesspanne: die Spanne eines Tages enthaelt den Spread einmal, die
# Zweitagesspanne auch nur einmal -- aber doppelt so viel echte Volatilitaet.
# Aus dieser Asymmetrie laesst sich der Spread herausrechnen.
#
# Der Schaetzer ist grob (er ignoriert Ueberachtluecken und wird bei sehr
# illiquiden Titeln unzuverlaessig), aber er ist der Standard, wenn man nur
# OHLC-Daten hat. Fuer die Frage "sind die Handelskosten groesser als der
# Effekt?" reicht die Groessenordnung.
px[, `:=`(hl1 = log(high / low),
          hl2 = log(shift(high, -1) / shift(low, -1))), by = id]
px[, gamma_cs := log(pmax(high, shift(high, -1)) /
                     pmin(low,  shift(low,  -1)))^2, by = id]
px[, beta_cs := hl1^2 + hl2^2]
k1 <- 3 - 2 * sqrt(2)
px[, alpha_cs := (sqrt(2 * beta_cs) - sqrt(beta_cs)) / k1 - sqrt(gamma_cs / k1)]
px[, spread_cs := 2 * (exp(alpha_cs) - 1) / (1 + exp(alpha_cs))]
px[!is.finite(spread_cs) | spread_cs < 0, spread_cs := 0]  # negative Schaetzungen
                                                           # auf 0, so der
                                                           # Vorschlag der Autoren
px[spread_cs > 0.20, spread_cs := NA_real_]                # >20% ist Unsinn
px[, c("hl1", "hl2", "gamma_cs", "beta_cs", "alpha_cs") := NULL]

# --- Laufender Index je Ticker ----------------------------------------------
setorder(px, id, date)
px[, t_idx := seq_len(.N), by = id]
px[, n_id  := .N, by = id]

msg("Kurs-Panel: ", format(nrow(px), big.mark = ".", decimal.mark = ","), " Zeilen, ",
    uniqueN(px$id), " Ticker.")
msg("Verworfene Tagesrenditen (Spruenge / Bereinigungsfehler): ",
    px[!is.na(ret_raw) & is.na(ret), .N])

# =============================================================================
# 3. LEBEN UND STERBEN JE TICKER
# =============================================================================
# Endet eine Kursreihe vor dem Datenende, ist der Titel verschwunden
# (Delisting laut RIC oder "still", z.B. nach einer Uebernahme). Eine offene
# Position wird dann zum LETZTEN verfuegbaren Kurs geschlossen -- genau das
# passiert in der Praxis bei einer Uebernahme oder Einstellung des Handels.
#
# ANNAHME: Bei einer Pleite ist der letzte Yahoo-Kurs eher zu hoch (der
# Restwert danach fehlt). Das schoent die Verlierer etwas.

life <- px[, .(first_px = min(date), last_px = max(date), n_px = .N), by = id]
life <- merge(universe[, .(id, name, market, delist_date, is_dead)],
              life, by = "id", all.x = TRUE)
data_end <- max(px$date)
life[, verschwunden := !is.na(last_px) & last_px < data_end - 21]

msg("Lebenszyklus: ", sum(!is.na(life$delist_date)), " RIC-Delistings, ",
    life[verschwunden == TRUE, .N], " Kursreihen enden vor dem Datenende.")

# =============================================================================
# 4. EREIGNISSE
# =============================================================================
# Ein Ereignis ist ein Quartalsbericht mit einem ECHTEN Berichtsdatum.
#
# Imputierte Termine (Quartalsende + 75 Tage aus Skript 01) fliegen hier raus.
# Begruendung: bei einer Ereignisstudie ist der Zeitpunkt die Messachse. Ein
# um zwei Wochen falsch gesetzter Tag 0 verschmiert die Reaktion ueber das
# ganze Fenster und drueckt jeden Effekt gegen null. Lieber weniger Ereignisse
# als verrauschte.

ev <- fund[!is.na(report_date) & report_date > period_end,
           .(id, period_end, report_date, known_from, industry, market,
             mktcap_usd, eps_actual, eps_mean_est, eps_smart_est,
             sue, eps_surprise, pred_surprise, rev_surprise,
             surp_hit_rate4, rec_mean, price_target,
             rev_yoy, eps_yoy, roe, pe, net_margin, fcf_margin, debt_equity)]

msg("Quartale gesamt: ", nrow(fund),
    " | mit echtem Berichtsdatum: ", nrow(ev))

# --- Tag 0 finden -----------------------------------------------------------
# Kopie der Datumsspalte: in einem rollierenden Join wird die Join-Spalte
# selbst mit dem i-Wert (dem Berichtsdatum) gefuellt. Wir wollen aber den
# gefundenen HANDELSTAG, also fuehren wir ihn als eigene Spalte mit.
px[, px_date := date]
setkey(px, id, date)
hit <- px[ev[, .(id, date = report_date)], on = .(id, date), roll = -Inf,
          .(t0 = t_idx, d0 = px_date, nid = n_id)]
ev[, t0      := hit$t0]
ev[, date0   := hit$d0]
ev[, n_id    := hit$nid]
ev[, gap_d0  := as.numeric(date0 - report_date)]   # Tage bis zum naechsten Handel

# Liegt der naechste Handelstag mehr als 7 Tage nach dem Bericht, stimmt etwas
# nicht (Handelsaussetzung, Datenluecke). Raus.
ev <- ev[!is.na(t0) & gap_d0 <= 7]

ev[, n_pre  := t0 - 1L]
ev[, n_post := n_id - t0]
msg("Ereignisse mit Kursanschluss: ", nrow(ev))

ev <- merge(ev, life[, .(id, verschwunden)], by = "id", all.x = TRUE)
ev[, eid := .I]   # eindeutige Ereignis-Nummer

# --- Kalenderquartal des Ereignisses ----------------------------------------
# Dient spaeter als Cluster fuer die Standardfehler: Berichte derselben
# Saison erleben denselben Marktverlauf und sind nicht unabhaengig.
ev[, jahr    := as.integer(format(date0, "%Y"))]
ev[, quartal := paste0(jahr, "Q", (as.integer(format(date0, "%m")) - 1L) %/% 3L + 1L)]

# =============================================================================
# 5. EREIGNISZEIT-MATRIX
# =============================================================================
# Fuer jedes Ereignis holen wir die Handelstage t0 + k mit k von -60 bis +60.
# Das ist der teuerste Schritt (ca. 120 Zeilen je Ereignis), laeuft aber als
# ein einziger Join.

grid <- CJ(eid = ev$eid, k = EV$pre:EV$post)
grid <- merge(grid, ev[, .(eid, id, t0)], by = "eid")
grid[, t_idx := t0 + k]

pxk <- px[, .(id, t_idx, date, ar, ret, adv63_usd, spread_cs, close)]
setkey(pxk, id, t_idx)
et <- pxk[grid, on = .(id, t_idx)]
setorder(et, eid, k)

msg("Ereigniszeit-Matrix: ", format(nrow(et), big.mark = ".", decimal.mark = ","), " Zeilen.")

# --- Vor-Ereignis-Volatilitaet ----------------------------------------------
# Streuung der Abnormalrenditen im Fenster -60 bis -11, spaeter hochgerechnet
# auf die Laenge des Driftfensters. Sie ist der Nenner, mit dem wir CAR spaeter
# vergleichbar machen: 3% Drift bei einem ruhigen Versorger sind ein anderes
# Ereignis als 3% bei einem Biotech.
#
# Das Fenster endet bei -11 und nicht bei -1, damit die Vorlaufbewegung
# (Leaks, Analystenrevisionen kurz vor dem Termin) die Normierung nicht
# aufblaeht.
#
# LOOKAHEAD-CHECK: benutzt nur Daten VOR dem Ereignis. Sauber.
vol <- et[k >= EV$win_vol[1] & k <= EV$win_vol[2] & !is.na(ar),
          .(pre_vol_d = sd(ar), n_vol = .N), by = eid]
vol[n_vol < 30L, pre_vol_d := NA_real_]

# --- Ein- und Ausstieg je Ereignis -----------------------------------------
# Einstieg: Schlusskurs an Tag entry_lag. Ausstieg: erster Handelstag, der
# mindestens hold_days Kalendertage nach dem Einstieg liegt. Das Driftfenster
# ist alles dazwischen, also Tag entry_lag + 1 bis Ausstieg.
entry <- et[k == CFG$entry_lag & !is.na(date), .(eid, date_entry = date)]
et <- merge(et, entry, by = "eid", all.x = TRUE)
exit <- et[k > CFG$entry_lag & !is.na(date) &
             date >= date_entry + CFG$hold_days,
           .(k_exit = min(k)), by = eid]
et <- merge(et, exit, by = "eid", all.x = TRUE)

# Kein Ausstiegstag gefunden heisst: die Kursreihe endet vorher. Ist der Titel
# verschwunden, endet die Position am letzten Kurs (k_last). Laeuft er noch,
# ist das Haltefenster schlicht noch nicht vorbei -> kein Ergebnis.
last <- et[!is.na(date), .(k_last = max(k)), by = eid]
et <- merge(et, last, by = "eid", all.x = TRUE)
et <- merge(et, ev[, .(eid, verschwunden)], by = "eid", all.x = TRUE)
et[, exit_frueh := is.na(k_exit) & verschwunden == TRUE & k_last > CFG$entry_lag]
et[exit_frueh == TRUE, k_exit := k_last]
et[, drift := k > CFG$entry_lag & !is.na(k_exit) & k <= k_exit]

# --- Fensterrenditen ---------------------------------------------------------
agg <- et[, .(
  date_entry = date_entry[1],
  k_exit    = k_exit[1],
  exit_frueh= exit_frueh[1],
  car_ann   = sum(ar[k >= EV$win_ann[1]   & k <= EV$win_ann[2]],   na.rm = TRUE),
  n_ann     = sum(!is.na(ar[k >= EV$win_ann[1]   & k <= EV$win_ann[2]])),
  car90     = sum(ar[drift], na.rm = TRUE),
  n_drift   = sum(!is.na(ar[drift])),
  car_run   = sum(ar[k >= EV$win_run[1]   & k <= EV$win_run[2]],   na.rm = TRUE),
  car20     = sum(ar[k > CFG$entry_lag & k <= 20L], na.rm = TRUE),
  ret90_roh = sum(ret[drift], na.rm = TRUE),
  adv_pre   = median(adv63_usd[k >= -20L & k <= -2L], na.rm = TRUE),
  spread_pre= median(spread_cs[k >= -60L & k <= -2L], na.rm = TRUE),
  px0       = close[k == 0L][1]
), by = eid]

# Haltefenster nicht abgeschlossen (laufender Titel, Datenende) -> NA.
agg[is.na(k_exit) | n_drift == 0L, car90 := NA_real_]
agg[n_ann < 2L, car_ann := NA_real_]

ev <- merge(ev, agg, by = "eid", all.x = TRUE)
ev <- merge(ev, vol[, .(eid, pre_vol_d, n_vol)], by = "eid", all.x = TRUE)

# exit_frueh = Titel vor Ablauf der 90 Tage verschwunden. Markiert, NICHT
# geloescht: wer diese Ereignisse wegwirft, wirft genau die Pleiten und
# Uebernahmen weg.

# --- Standardisiertes CAR ----------------------------------------------------
# car90_std = "wie viele Vor-Ereignis-Standardabweichungen ist der Kurs
# gelaufen". Ohne diese Normierung dominieren volatile Nebenwerte jede
# Mittelwertsrechnung, und du haelst deren Rauschen fuer einen Effekt.
ev[, pre_event_vol := pre_vol_d * sqrt(pmax(k_exit - CFG$entry_lag, 1L))]
ev[, car90_std   := car90   / pre_event_vol]
ev[, car_ann_std := car_ann / (pre_vol_d * sqrt(3))]

# =============================================================================
# 6. RAENGE -- NUR GEGEN BEREITS BEKANNTE BERICHTE
# =============================================================================
# Ein SUE von 2 bedeutete 2020 etwas anderes als 2023 -- die Streuung der
# Ueberraschungen schwankt mit der Unsicherheit im Markt. Deshalb ranken wir
# statt global zu standardisieren.
#
# LOOKAHEAD-FALLE: Frueher wurde je Kalenderquartal gerankt. Ein Bericht vom
# 5. Januar wurde dabei gegen Berichte vom 20. Februar verglichen, die am
# 5. Januar noch niemand kannte. Jetzt wird jeder Bericht nur gegen die
# Berichte der letzten CFG$rank_window_days Tage bis einschliesslich seines
# eigenen Tages gerankt -- alles davon ist beim Einstieg (Tag +1) bekannt.

#' Perzentilrang (0 bis 1) gegen ein rueckwaertsgerichtetes Zeitfenster
rank_trailing <- function(x, d, days = CFG$rank_window_days,
                          min_n = CFG$rank_min_n) {
  out <- rep(NA_real_, length(x))
  ok  <- which(!is.na(x) & !is.na(d))
  o   <- ok[order(d[ok])]
  dd  <- as.numeric(d[o]); xx <- x[o]
  for (day in unique(dd)) {
    win <- sort(xx[dd > day - days & dd <= day])
    if (length(win) < min_n) next
    me  <- which(dd == day)
    lo  <- findInterval(xx[me], win, left.open = TRUE)  # Anzahl < x
    hi  <- findInterval(xx[me], win)                    # Anzahl <= x
    out[o[me]] <- (lo + hi + 1) / 2 / length(win)       # wie frank(average)/n
  }
  out
}
q5 <- function(r) as.integer(cut(r, breaks = seq(0, 1, 0.2), labels = FALSE,
                                 include.lowest = TRUE))

ev[, sue_rank  := rank_trailing(sue, date0)]
ev[, sue_q5    := q5(sue_rank)]
ev[, ps_rank   := rank_trailing(pred_surprise, date0)]
ev[, ps_q5     := q5(ps_rank)]
ev[, revs_rank := rank_trailing(rev_surprise, date0)]

# Liquiditaetsdezil, ebenfalls rueckwaertsgerichtet. Absolute USD-Schwellen
# waeren falsch: 5 Mio Tagesumsatz sind 2019 und 2026 nicht dasselbe.
ev[, liq_dec := as.integer(cut(rank_trailing(adv_pre, date0),
                               breaks = seq(0, 1, 0.1), labels = FALSE,
                               include.lowest = TRUE))]

# =============================================================================
# 7. KONFUNDIERUNGS-KANDIDATEN
# =============================================================================
# Echte Guidance-Daten (Unternehmensprognosen) sind im LSEG-Export NICHT
# enthalten. Wir bauen drei Ersatzindikatoren, die dieselbe Frage von der
# Seite angehen: Kam am Berichtstag noch etwas anderes mit?

# a) Wie viele andere Firmen berichten am selben Tag? Hirshleifer, Lim und Teoh
#    zeigen: an vollen Tagen ist die Aufmerksamkeit knapp und die Unterreaktion
#    groesser. Wenn dein Drift NUR an solchen Tagen existiert, ist er ein
#    Aufmerksamkeitseffekt und kein Bewertungseffekt.
crowd <- ev[, .(crowd_n = .N), by = date0]
ev <- merge(ev, crowd, by = "date0", all.x = TRUE)

# b) Bestaetigt der Umsatz die Gewinnueberraschung? Eine Gewinnueberraschung,
#    die nur aus Kostensenkung stammt, ist weniger persistent als eine, die
#    vom Umsatz getragen wird.
ev[, rev_bestaetigt := !is.na(rev_surprise) & !is.na(eps_surprise) &
     sign(rev_surprise) == sign(eps_surprise)]

# c) Analystenempfehlung im Vergleich zum Vorquartal. Sinkt rec_mean (1 = Buy),
#    wurde hochgestuft. Das ist der naechstbeste Ersatz fuer eine Revision.
setorder(ev, id, period_end)
ev[, rec_chg := rec_mean - shift(rec_mean), by = id]

# =============================================================================
# 8. SPEICHERN
# =============================================================================

# Ereigniszeit auf das Fenster reduzieren, das die Grafiken brauchen.
# Nach dem Ausstieg zaehlt nichts mehr (ar = NA), damit der Pfad eines
# vorzeitig beendeten Ereignisses auf seinem letzten Wert stehen bleibt.
et_out <- et[k >= EV$path_from & k <= EV$post,
             .(eid, k, date, ar = fifelse(!is.na(k_exit) & k > k_exit, NA_real_, ar), ret)]

# CAR-Pfad kumulieren. NA-Tage zaehlen als 0 -- sonst bricht der Pfad an
# einem einzelnen fehlenden Kurs komplett ab.
et_out[, ar_f := fifelse(is.na(ar), 0, ar)]
setorder(et_out, eid, k)
et_out[, car_t := cumsum(ar_f), by = eid]
et_out[, ar_f := NULL]

saveRDS(ev,     file.path(CFG$dir_proc, "events.rds"))
saveRDS(et_out, file.path(CFG$dir_proc, "event_time.rds"))
px[, px_date := NULL]
saveRDS(px[, .(id, market, date, t_idx, close, adjclose, ret, ret_raw, ar,
               mkt_ret, adv63_usd, spread_cs, fx_usd)],
        file.path(CFG$dir_proc, "px_daily.rds"))
saveRDS(life,   file.path(CFG$dir_proc, "life.rds"))

msg("=== Fertig ===")
msg("Ereignisse            : ", nrow(ev))
msg("davon mit CAR90       : ", ev[!is.na(car90), .N],
    "  (davon vorzeitig beendet: ", ev[exit_frueh == TRUE, .N], ")")
msg("davon mit SUE         : ", ev[!is.na(sue), .N])
msg("davon mit beidem      : ", ev[!is.na(car90) & !is.na(sue), .N])
msg("Zeitraum              : ", format(min(ev$date0)), " bis ",
    format(max(ev$date0)))
msg("Ticker                : ", uniqueN(ev$id))
msg("Naechster Schritt     : 04_auswertung.R")
