# =============================================================================
# 05_diagnose_optional.R  --  Zusaetzliche Diagnosen zum Ereignispanel
# =============================================================================
#
# OPTIONAL. Die Kernauswertung steht in 04_auswertung.R. Hier liegen die
# Pruefungen, die man einmal gruendlich macht und dann nur noch bei
# Datenaenderungen wiederholt:
#
#   Teil 1  Datenintegritaet (Duplikate, Luecken, Meldeverzug, Ausreisser)
#   Teil 2  Fette Raender (Hill-Schaetzer) -- warum wir Mediane berichten
#   Teil 3  Warum Raenge statt globaler Standardisierung
#   Teil 4  Stabilitaet je Jahr
#   Teil 5  Autokorrelation der Ueberraschungen (der PEAD-Mechanismus)
#   Teil 9  Konfundierung (Umsatz, volle Berichtstage, Analystenrevisionen)
#
# ZU JEDEM TEST STEHT IM KOMMENTAR:
#   THEORIE    -- warum man das ueberhaupt prueft
#   ERGEBNIS?  -- wie du das Resultat liest, in beide Richtungen
#
# Voraussetzung: 01, 02 und 03 sind gelaufen.
# =============================================================================

source("00_config.R")

suppressPackageStartupMessages({
  library(ggplot2)
})

PLOT_DIR <- "diagnose"

# =============================================================================
# TEIL 0 -- LADEN, UEBERBLICK
# =============================================================================

ev  <- readRDS(file.path(CFG$dir_proc, "events.rds"))
et  <- readRDS(file.path(CFG$dir_proc, "event_time.rds"))
px  <- readRDS(file.path(CFG$dir_proc, "px_daily.rds"))
setDT(ev); setDT(et); setDT(px)

dir_plot <- file.path(CFG$dir_out, PLOT_DIR)
if (!dir.exists(dir_plot)) dir.create(dir_plot, recursive = TRUE)

# Der einzige Helfer im Skript: anzeigen und wegspeichern in einem Rutsch.
P <- function(plot_obj, name, w = 9, h = 5.5) {
  print(plot_obj)
  ggsave(file.path(dir_plot, paste0(name, ".png")), plot_obj,
         width = w, height = h, dpi = 130)
  invisible(NULL)
}

theme_set(theme_minimal(base_size = 11))

# Arbeitsdatensatz: nur Ereignisse, bei denen Signal UND Reaktion vorliegen.
# sue_q5 ist der rueckwaertsgerichtete Rang aus 03 (kein Lookahead).
d <- copy(ev[!is.na(car90) & !is.na(sue_rank)])

msg("=========================================================")
msg("Ereignisse insgesamt          : ", nrow(ev))
msg("mit CAR90 und SUE-Rang (Arbeitsset): ", nrow(d))
msg("Ticker im Arbeitsset          : ", uniqueN(d$id))
msg("Quartale                      : ", uniqueN(d$quartal))
msg("Zeitraum                      : ", format(min(d$date0)), " bis ",
    format(max(d$date0)))
msg("=========================================================")

# =============================================================================
# TEIL 1 -- DATENINTEGRITAET
# =============================================================================
# THEORIE: Jede spaetere Kennzahl ist eine Funktion dieser Rohdaten. Ein
# doppeltes Quartal verzerrt die Rangbildung, ein falscher Berichtstermin
# verschiebt die gesamte Ereigniszeit. Diese Fehler sind nicht klein und
# zufaellig, sondern gross und systematisch -- sie treffen typischerweise
# dieselben Titel (kleine, illiquide, schlecht abgedeckte).

msg("\n### TEIL 1: DATENINTEGRITAET ###\n")

# --- 1.1 Duplikate -----------------------------------------------------------
# Drei Arten, die alle vorkommen:
#   a) gleiches Quartalsende zweimal  -> Geschaeftsjahresumstellung im Export
#   b) gleicher Berichtstermin fuer zwei Quartale -> Nachmeldung/Datenfehler
#   c) exakt identische Zeile         -> Batch-Export doppelt eingesammelt
dup_pe <- ev[duplicated(ev, by = c("id", "period_end")) |
               duplicated(ev, by = c("id", "period_end"), fromLast = TRUE)]
dup_rd <- ev[duplicated(ev, by = c("id", "report_date")) |
               duplicated(ev, by = c("id", "report_date"), fromLast = TRUE)]

msg("Duplikate (id + period_end) : ", nrow(dup_pe))
msg("Duplikate (id + report_date): ", nrow(dup_rd), "  <- zwei Quartale, ",
    "ein Termin: pruefen!")
if (nrow(dup_rd)) print(head(dup_rd[order(id, period_end),
                                    .(id, period_end, report_date, eps_actual)], 12))

# ERGEBNIS? (a) sollte 0 sein, weil Skript 01 dedupliziert. (b) darf in
# Einzelfaellen vorkommen (Doppelbericht nach Umstellung), aber nicht in
# hunderten Faellen. Wenn doch: die Ereigniszeit ist dann fuer beide Quartale
# identisch und du zaehlst dieselbe Kursreaktion doppelt.

# --- 1.2 Fehlende Werte, Spalte fuer Spalte ---------------------------------
cols_chk <- c("eps_actual", "eps_mean_est", "eps_smart_est", "sue",
              "eps_surprise", "pred_surprise", "rev_surprise", "rec_mean",
              "car_ann", "car90", "pre_event_vol", "adv_pre", "spread_pre",
              "mktcap_usd", "roe", "pe")
na_tab <- data.table(
  spalte = cols_chk,
  na_pct = sapply(cols_chk, function(cc) round(100 * mean(is.na(ev[[cc]])), 1))
)[order(-na_pct)]
print(na_tab)

P(ggplot(na_tab, aes(reorder(spalte, na_pct), na_pct)) +
    geom_col(fill = "grey40") + coord_flip() +
    labs(title = "Anteil fehlender Werte je Spalte",
         x = NULL, y = "% NA"),
  "01_na_je_spalte")

# --- 1.3 Fehlende Werte ueber die ZEIT --------------------------------------
# THEORIE: Ein konstanter NA-Anteil ist ein Datenqualitaetsproblem. Ein NA-
# Anteil, der sich ueber die Zeit VERAENDERT, ist ein Selektionsproblem. Wenn
# die Estimate-Abdeckung 2019 bei 60% lag und 2025 bei 95%, vergleichst du
# ueber die Jahre nicht dieselben Firmen -- und jede Aussage ueber "der Effekt
# nimmt ab" ist womoeglich ein Abdeckungseffekt.
cov_t <- ev[, .(sue        = mean(!is.na(sue)),
                pred_surp  = mean(!is.na(pred_surprise)),
                car90      = mean(!is.na(car90)),
                spread     = mean(!is.na(spread_pre))), by = quartal]
cov_t <- melt(cov_t, id.vars = "quartal")
P(ggplot(cov_t, aes(quartal, 100 * value, colour = variable, group = variable)) +
    geom_line(linewidth = 0.8) + geom_point(size = 1) +
    labs(title = "Abdeckung je Quartal",
         subtitle = "Faellt eine Linie in der Mitte ab, aendert sich das Sample",
         x = NULL, y = "% vorhanden", colour = NULL) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)),
  "02_abdeckung_ueber_zeit")

# --- 1.4 Quartalsrhythmus ----------------------------------------------------
# THEORIE: Wenn zwischen zwei Quartalsenden 180 statt 92 Tage liegen, fehlt
# ein Quartal. Die rollierenden Kennzahlen aus Skript 01 (TTM, SUE-Streuung
# ueber 8 Quartale) summieren dann ueber ein Loch hinweg.
ev2 <- copy(ev); setorder(ev2, id, period_end)
ev2[, dq := as.numeric(period_end - shift(period_end)), by = id]
P(ggplot(ev2[!is.na(dq) & dq < 400], aes(dq)) +
    geom_histogram(binwidth = 5, fill = "grey40") +
    geom_vline(xintercept = c(92, 183), colour = "firebrick", linetype = 2) +
    labs(title = "Abstand zwischen zwei Quartalsenden",
         subtitle = "Linien: 92 Tage (Quartal) und 183 Tage (Halbjahr)",
         x = "Kalendertage", y = "Anzahl"),
  "03_quartalsrhythmus")
msg("Quartalsluecken > 130 Tage: ", ev2[dq > 130, .N],
    " (davon Halbjahresberichter, typisch in HK und DE)")

# --- 1.5 Meldeverzug ---------------------------------------------------------
# THEORIE: Der Abstand Quartalsende zu Bericht sollte je Markt eine enge
# Verteilung haben (US ~30 Tage, Japan ~40, HK-Halbjahresberichter ~60).
# Ein Bericht 5 Tage nach Quartalsende ist unmoeglich, einer nach 150 Tagen
# ist entweder ein Nachzuegler oder ein Datenfehler.
ev[, lag_report := as.numeric(report_date - period_end)]
P(ggplot(ev[lag_report < 200], aes(lag_report)) +
    geom_histogram(binwidth = 3, fill = "grey40") +
    facet_wrap(~market, scales = "free_y") +
    labs(title = "Meldeverzug: Bericht minus Quartalsende",
         x = "Kalendertage", y = "Anzahl"),
  "04_meldeverzug", w = 11, h = 7)
print(ev[, .(median_lag = median(lag_report, na.rm = TRUE),
             p95 = quantile(lag_report, .95, na.rm = TRUE),
             unter_10d = sum(lag_report < 10, na.rm = TRUE),
             ueber_120d = sum(lag_report > 120, na.rm = TRUE)),
         by = market][order(-median_lag)])

# ERGEBNIS? Faelle unter 10 Tagen sind fast sicher Datenfehler und verdienen
# einen Blick. Sie sind gefaehrlich, weil sie den Tag 0 zu frueh setzen: das
# Ankuendigungsfenster faengt die Reaktion dann gar nicht ein, und die echte
# Reaktion landet mitten im Driftfenster. Das ERZEUGT scheinbaren Drift.

# --- 1.6 Ausreisser in den Signalen -----------------------------------------
# THEORIE: eps_surprise und pred_surprise haben eine Schaetzung im Nenner.
# Naehert die sich null, explodiert der Quotient. Deshalb wurde in Skript 01
# zusaetzlich SUE gebaut, das durch die Streuung der eigenen Surprise-Historie
# teilt statt durch das Niveau.
sig_long <- melt(ev[, .(eid, sue, eps_surprise, pred_surprise, rev_surprise)],
                 id.vars = "eid", na.rm = TRUE)
print(sig_long[, .(min = round(min(value), 1),
                   p1  = round(quantile(value, .01), 2),
                   median = round(median(value), 2),
                   p99 = round(quantile(value, .99), 2),
                   max = round(max(value), 1)), by = variable])

P(ggplot(sig_long[abs(value) < 200], aes(value)) +
    geom_histogram(bins = 80, fill = "grey40") +
    facet_wrap(~variable, scales = "free") +
    labs(title = "Verteilung der Signale (auf +/-200 beschnitten)",
         x = NULL, y = "Anzahl"),
  "05_signalverteilung")

# ERGEBNIS? Wenn eps_surprise Werte von mehreren tausend Prozent zeigt und SUE
# nicht, hast du den Beleg dafuer, warum SUE das bessere Mass ist. Genau
# deshalb wird in der Literatur mit SUE und nicht mit Prozentabweichung
# gearbeitet.

# --- 1.7 Ausreisser in CAR --------------------------------------------------
P(ggplot(ev[!is.na(car90)], aes(quartal, car90)) +
    geom_boxplot(outlier.size = 0.4, outlier.alpha = 0.3) +
    coord_cartesian(ylim = c(-1, 1)) +
    geom_hline(yintercept = 0, colour = "firebrick") +
    labs(title = "CAR90 je Quartal", x = NULL,
         y = "kumulierte Abnormalrendite Tag +2 bis +60") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)),
  "06_car90_boxplot")

msg("Extremste CAR90-Werte:")
print(head(ev[order(-abs(car90)), .(id, market, period_end, report_date,
                                    car90 = round(car90, 2), sue = round(sue, 1),
                                    exit_frueh, n_drift)], 15))

# ERGEBNIS? Ein CAR90 von +300% ist kein Drift, sondern eine Uebernahme, ein
# Reverse Split oder ein Kursfehler. Schau dir die Liste an. Wenn dieselben
# Titel mehrfach auftauchen, hast du ein Ticker-Problem und keinen Ausreisser.

# --- 1.8 Preisdaten-Sanity ---------------------------------------------------
msg("Tagesrenditen ueber +100% / unter -80% (in 03 auf NA gesetzt): ",
    px[!is.na(ret_raw) & is.na(ret), .N])
msg("Ereignisse, deren Titel vor Ablauf der 90 Tage verschwand: ",
    ev[exit_frueh == TRUE, .N])
# THEORIE: Abgeschnittene Fenster sind der Einstieg fuer Survivorship Bias
# durch die Hintertuer. Wenn du sie einfach wegwirfst, verschwinden genau die
# Pleiten. Deshalb rechnet Skript 03 sie bis zum letzten verfuegbaren Kurs.


# =============================================================================
# TEIL 2 -- FAT TAILS UND TAIL-INDEX (HILL-SCHAETZER)
# =============================================================================
# THEORIE, kurz und in Worten:
# Der t-Test und alles, was darauf aufbaut, unterstellt, dass der Mittelwert
# vieler Beobachtungen gegen eine Normalverteilung strebt. Das gilt nur, wenn
# die Varianz endlich ist. Bei Renditeverteilungen ist das nicht garantiert.
#
# Der Tail-Index alpha beschreibt, wie schnell die Wahrscheinlichkeit extremer
# Werte abfaellt. Faustregel:
#   alpha > 4  -> Varianz und Kurtosis existieren, Mittelwert ist brauchbar
#   2 < alpha < 4 -> Varianz existiert, Kurtosis nicht. t-Werte konvergieren
#                    sehr langsam, sind bei kleinen Samples zu optimistisch
#   alpha < 2  -> unendliche Varianz. Der Stichprobenmittelwert konvergiert
#                 gegen nichts. Ein Mittelwert-t-Test ist dann bedeutungslos
#
# Der Hill-Schaetzer misst alpha aus den k groessten Beobachtungen. Er ist
# empfindlich gegenueber der Wahl von k -- deshalb zeichnen wir ihn ueber
# einen ganzen Bereich von k (den "Hill-Plot") und schauen, ob es einen
# stabilen Bereich gibt.

msg("\n### TEIL 2: FAT TAILS ###\n")

hill <- function(x, k) {
  x <- x[is.finite(x) & x > 0]
  n <- length(x)
  if (n < 50 || k >= n) return(NA_real_)
  xs <- sort(x, decreasing = TRUE)
  1 / mean(log(xs[1:k] / xs[k + 1]))
}

# --- 2.1 Wie weit weg von der Normalverteilung? ------------------------------
x <- d$car90
msg("CAR90: Mittelwert ", round(mean(x), 4),
    " | Median ", round(median(x), 4),
    " | SD ", round(sd(x), 3),
    " | Schiefe ", round(mean((x - mean(x))^3) / sd(x)^3, 2),
    " | Kurtosis ", round(mean((x - mean(x))^4) / sd(x)^4, 1))
msg("  (Normalverteilung haette Schiefe 0 und Kurtosis 3)")

P(ggplot(d, aes(sample = car90)) +
    stat_qq(size = 0.4, alpha = 0.4) + stat_qq_line(colour = "firebrick") +
    labs(title = "QQ-Plot: CAR90 gegen Normalverteilung",
         subtitle = "S-Form an den Enden = fette Raender",
         x = "theoretische Quantile", y = "beobachtet"),
  "07_qqplot_car90")

# --- 2.2 Hill-Plot, gepoolt --------------------------------------------------
ks <- unique(round(seq(30, max(50, floor(0.20 * nrow(d))), length.out = 60)))
hp <- rbindlist(list(
  data.table(k = ks, alpha = sapply(ks, function(kk) hill( d$car90, kk)), rand = "rechts"),
  data.table(k = ks, alpha = sapply(ks, function(kk) hill(-d$car90, kk)), rand = "links")
))
P(ggplot(hp[is.finite(alpha)], aes(k, alpha, colour = rand)) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = c(2, 4), linetype = 2, colour = "grey30") +
    labs(title = "Hill-Plot des Tail-Index von CAR90",
         subtitle = "unter 2 = unendliche Varianz | unter 4 = unendliche Kurtosis",
         x = "k (Zahl der beruecksichtigten Extremwerte)",
         y = "alpha", colour = "Rand"),
  "08_hillplot")

# --- 2.3 Tail-Index je Liquiditaetsdezil -------------------------------------
# THEORIE: Illiquide Titel springen. Ein einziger Sprung von +80% in Dezil 1
# kann den Mittelwert des gesamten Dezils tragen. Genau das ist der Mechanismus,
# ueber den Micro Caps in der Literatur den PEAD "am Leben halten".
tail_dec <- d[!is.na(liq_dec), {
  n <- .N
  k <- max(20L, floor(0.05 * n))
  .(n = n,
    alpha_re = hill( car90, k),
    alpha_li = hill(-car90, k),
    mittel   = mean(car90),
    median   = median(car90),
    getrimmt = mean(car90, trim = 0.05),
    sd       = sd(car90))
}, by = liq_dec][order(liq_dec)]
print(tail_dec[, lapply(.SD, function(z) if (is.numeric(z)) round(z, 3) else z)])

P(ggplot(melt(tail_dec[, .(liq_dec, alpha_re, alpha_li)], id.vars = "liq_dec"),
         aes(factor(liq_dec), value, fill = variable)) +
    geom_col(position = "dodge") +
    geom_hline(yintercept = c(2, 4), linetype = 2) +
    labs(title = "Tail-Index je Liquiditaetsdezil",
         subtitle = "Dezil 1 = illiquideste Ereignisse",
         x = "Liquiditaetsdezil", y = "alpha (Hill)", fill = NULL),
  "09_tailindex_je_dezil")

# --- 2.4 Der eigentliche Punkt: Mittelwert gegen Median ----------------------
P(ggplot(melt(tail_dec[, .(liq_dec, mittel, median, getrimmt)],
              id.vars = "liq_dec"),
         aes(factor(liq_dec), value, fill = variable)) +
    geom_col(position = "dodge") +
    geom_hline(yintercept = 0) +
    labs(title = "CAR90 je Liquiditaetsdezil: Mittelwert vs. Median",
         subtitle = "Klaffen die Balken auseinander, treiben Ausreisser den Mittelwert",
         x = "Liquiditaetsdezil", y = "CAR90", fill = NULL),
  "10_mittel_vs_median")

# ERGEBNIS? Wenn Mittelwert und Median in den unteren Dezilen weit
# auseinanderliegen und alpha dort unter 3 faellt, ist die Konsequenz nicht
# verhandelbar: ab hier wird der MEDIAN berichtet, und Mittelwerte tauchen
# hoechstens als Nebenbemerkung auf. Fallen die Balken zusammen, kannst du
# beim Mittelwert bleiben -- dann sind auch die t-Werte aus Teil 8 belastbarer.


# =============================================================================
# TEIL 3 -- QUERSCHNITTS-NORMALISIERUNG STATT GLOBALER STANDARDISIERUNG
# =============================================================================
# THEORIE: Zwei getrennte Probleme, zwei getrennte Loesungen.
#
#   Problem 1: Das SIGNAL ist ueber die Zeit nicht vergleichbar. Die Streuung
#   der Gewinnueberraschungen war 2020 ein Vielfaches von 2019. Wer global
#   standardisiert, sortiert Krisenquartale ins Extremquintil.
#   -> Loesung: Raenge gegen die Berichte der letzten 90 Tage. Danach
#      liegt in jedem Zeitraum etwa ein Fuenftel im Top-Quintil.
#
#   Problem 2: Die REAKTION ist ueber Titel nicht vergleichbar. 4% Drift bei
#   Nestle und 4% bei einem koreanischen Nebenwert sind nicht dasselbe
#   Ereignis.
#   -> Loesung: durch die Vor-Ereignis-Volatilitaet teilen (car90_std).
#
# Beide Groessen sind in Skript 03 bereits gebaut. Hier zeigen wir, warum.

msg("\n### TEIL 3: NORMALISIERUNG ###\n")

# --- 3.1 Wandert die SUE-Verteilung ueber die Zeit? --------------------------
P(ggplot(d, aes(quartal, sue)) +
    geom_boxplot(outlier.alpha = 0.15, outlier.size = 0.3) +
    coord_cartesian(ylim = c(-6, 6)) +
    geom_hline(yintercept = 0, colour = "firebrick") +
    labs(title = "ROH: SUE je Quartal",
         subtitle = "Verschiebt sich der Median, ist ein globaler z-Score verzerrt",
         x = NULL, y = "SUE") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)),
  "11_sue_roh_je_quartal")

print(d[, .(median_sue = round(median(sue), 2),
            iqr_sue = round(IQR(sue), 2),
            anteil_positiv = round(mean(sue > 0), 2)), by = quartal])

# ERGEBNIS? Der Anteil positiver Ueberraschungen liegt in der Praxis dauerhaft
# ueber 50% -- Analysten setzen die Latte bewusst niedrig, Unternehmen
# uebertreffen sie. Das ist kein Fehler in den Daten, sondern der Grund, warum
# ein absoluter Filter wie "eps_surprise > 0" fast nichts aussortiert und ein
# RANG ihn ersetzen muss.

# --- 3.2 Nach Rangbildung ----------------------------------------------------
P(ggplot(d, aes(quartal, sue_rank)) +
    geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.3) +
    labs(title = "NORMALISIERT: SUE-Perzentilrang je Quartal",
         subtitle = "Rueckwaertsgerichteter Rang: je Quartal annaehernd gleich verteilt",
         x = NULL, y = "Rang (0 bis 1)") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)),
  "12_sue_rang_je_quartal")

# --- 3.3 Vor-Ereignis-Volatilitaet als Nenner -------------------------------
P(ggplot(d[!is.na(pre_event_vol) & pre_event_vol < 1],
         aes(pre_event_vol, abs(car90))) +
    geom_point(size = 0.3, alpha = 0.15) +
    geom_smooth(method = "lm", formula = y ~ x, colour = "firebrick") +
    labs(title = "Betrag des CAR90 gegen Vor-Ereignis-Volatilitaet",
         subtitle = "Steigende Gerade = volatile Titel liefern grosse CARs, unabhaengig vom Signal",
         x = "erwartete Streuung ueber das Haltefenster", y = "|CAR90|"),
  "13_car_vs_vol")

msg("Korrelation |CAR90| ~ pre_event_vol: ",
    round(cor(abs(d$car90), d$pre_event_vol, use = "complete.obs"), 3))

# ERGEBNIS? Eine deutliche positive Korrelation ist zu erwarten und harmlos --
# sie ist genau der Grund fuer die Normierung. Problematisch wird es erst,
# wenn die Vor-Ereignis-Volatilitaet auch mit dem SIGNAL korreliert. Dann
# vermengen sich Effekt und Risiko:
msg("Korrelation SUE-Rang ~ pre_event_vol: ",
    round(cor(d$sue_rank, d$pre_event_vol, use = "complete.obs"), 3))


# =============================================================================
# TEIL 4 -- STABILITAET DES EFFEKTS UEBER DIE ZEIT
# =============================================================================
# THEORIE: Der bekannteste Einwand gegen PEAD ist, dass er seit den 2000er
# Jahren verschwunden ist -- fuer grosse Titel praktisch vollstaendig. Die
# Begruendungen reichen von "wegarbitriert" bis "das Signal selbst ist
# schwaecher geworden". Fuer dich ist beides gleich relevant: wenn der Effekt
# in deinem Fenster von 2019 bis heute schrumpft, ist ein Backtest ueber den
# gesamten Zeitraum eine Mittelung ueber zwei verschiedene Welten.
#
# ACHTUNG bei der Interpretation: Bei rund 28 Quartalen und einem Effekt in
# der Groessenordnung von ein bis zwei Prozent hat ein Jahresvergleich sehr
# wenig statistische Kraft. Ein fallender Verlauf ist ein HINWEIS, kein Beweis.

msg("\n### TEIL 4: STABILITAET ###\n")

stab <- d[!is.na(sue_q5), .(n = .N,
                            median_car = median(car90),
                            mittel_car = mean(car90),
                            median_std = median(car90_std, na.rm = TRUE)),
          by = .(jahr, sue_q5)][order(jahr, sue_q5)]
print(dcast(stab, jahr ~ sue_q5, value.var = "median_car")[
  , lapply(.SD, function(z) if (is.numeric(z)) round(z, 4) else z)])

P(ggplot(stab, aes(jahr, median_car, colour = factor(sue_q5))) +
    geom_line(linewidth = 0.9) + geom_point() +
    geom_hline(yintercept = 0) +
    labs(title = "Median-CAR90 je Jahr und SUE-Quintil",
         subtitle = "Quintil 5 = groesste positive Ueberraschung",
         x = NULL, y = "Median CAR90", colour = "SUE-Quintil"),
  "14_stabilitaet_jahr")

# =============================================================================
# TEIL 5 -- AUTOKORRELATION DER SUE-REIHE JE AKTIE
# =============================================================================
# THEORIE, und das ist der Kern des ganzen Skripts:
# PEAD ist keine Kursanomalie, die man einfach beobachtet. Sie hat einen
# behaupteten Mechanismus. Bernard und Thomas haben ihn 1990 formuliert:
# Quartalsueberraschungen sind AUTOKORRELIERT. Wer dieses Quartal positiv
# ueberrascht, ueberrascht mit erhoehter Wahrscheinlichkeit auch naechstes
# Quartal positiv. Anleger behandeln jede Ueberraschung faelschlich als
# Einmaleffekt und werden vom naechsten Bericht erneut ueberrascht. Der
# Kursdrift ist die Vorwegnahme dieser kommenden Ueberraschung.
#
# Das erzeugt ein charakteristisches Muster:
#   Lag 1, 2, 3 -> positive Autokorrelation, abnehmend
#   Lag 4       -> NEGATIV (nach vier Quartalen ist der Vergleich mit dem
#                  Vorjahresquartal, in dem die Ueberraschung schon steckte)
#
# Wenn dieses Muster in deinen Daten fehlt, fehlt der Mechanismus. Dann kann
# es zwar trotzdem einen Kursdrift geben -- aber dann ist es nicht der Drift,
# den die Literatur beschreibt, und du solltest ihn anders nennen und anders
# begruenden.
#
# WICHTIG: Wir rechnen die Autokorrelation der SUE-REIHE, nicht der Kurse.
# Kursautokorrelation waere Momentum und eine ganz andere Frage.

msg("\n### TEIL 5: AUTOKORRELATION DER UEBERRASCHUNGEN ###\n")

sue_ser <- ev[!is.na(sue), .(id, period_end, sue)]
setorder(sue_ser, id, period_end)
sue_ser[, n_id := .N, by = id]

acf_dt <- sue_ser[n_id >= 12, {
  a <- stats::acf(sue, lag.max = 8, plot = FALSE, na.action = na.pass,
                  demean = TRUE)$acf[-1]
  .(lag = 1:8, ac = as.numeric(a))
}, by = id]

acf_pool <- acf_dt[is.finite(ac), .(
  n_ticker = .N,
  mittel   = mean(ac),
  median   = median(ac),
  se       = sd(ac) / sqrt(.N),
  t_wert   = mean(ac) / (sd(ac) / sqrt(.N))
), by = lag][order(lag)]
print(acf_pool[, lapply(.SD, function(z) round(z, 3))])

P(ggplot(acf_dt, aes(factor(lag), ac)) +
    geom_hline(yintercept = 0, colour = "firebrick") +
    geom_boxplot(outlier.alpha = 0.1, outlier.size = 0.3) +
    stat_summary(fun = mean, geom = "point", colour = "steelblue", size = 2.5) +
    labs(title = "Autokorrelation der SUE-Reihe je Ticker",
         subtitle = "blauer Punkt = Mittelwert ueber alle Ticker | erwartetes Muster: Lag 1-3 positiv, Lag 4 negativ",
         x = "Lag (Quartale)", y = "Autokorrelation"),
  "16_acf_sue")

P(ggplot(acf_pool, aes(factor(lag), mittel)) +
    geom_col(fill = "grey40") +
    geom_errorbar(aes(ymin = mittel - 1.96 * se, ymax = mittel + 1.96 * se),
                  width = 0.3) +
    geom_hline(yintercept = 0, colour = "firebrick") +
    labs(title = "Gepoolte Autokorrelation der Ueberraschungen",
         subtitle = "Fehlerbalken = 95% ueber die Streuung zwischen Tickern",
         x = "Lag (Quartale)", y = "mittlere Autokorrelation"),
  "17_acf_pooled")

# ERGEBNIS?
#   Lag 1 klar positiv, Lag 4 negativ  -> der Mechanismus ist da. Ein
#     gefundener Drift hat dann eine oekonomische Begruendung.
#   Alles nahe null -> entweder ist der Mechanismus in deinem Universum weg,
#     oder deine SUE-Konstruktion ist zu verrauscht (die Streuung ueber nur
#     8 Quartale ist ein grober Nenner). Beides musst du wissen, bevor du
#     einen Kursbefund als PEAD interpretierst.
#   Lag 1 stark NEGATIV -> deine Ueberraschungen kehren sich um. Das deutet
#     auf ein Konstruktionsproblem hin, etwa wenn eps_actual und eps_mean_est
#     unterschiedlich fuer Splits angepasst sind.


# =============================================================================
# TEIL 9 -- KONFUNDIERUNG
# =============================================================================
# THEORIE: Am Berichtstag kommt selten nur eine Nachricht. Unternehmen
# veroeffentlichen gleichzeitig ihre Prognose fuer die kommenden Quartale, und
# die Forschung zeigt, dass die Kursreaktion oft mehr an der Prognose haengt
# als an der berichteten Zahl. Wenn dein "Drift nach Ueberraschung" in
# Wahrheit ein "Drift nach Prognoseanhebung" ist, ist das eine andere
# Strategie mit anderen Voraussetzungen.
#
# ECHTE GUIDANCE-DATEN SIND IM LSEG-EXPORT NICHT ENTHALTEN.
# Wenn du sie brauchst: TR.GuidanceValue / TR.GuidanceMidPoint aus dem
# I/B/E/S-Guidance-Feed, das ist ein separater Abruf. Bis dahin arbeiten wir
# mit drei Ersatzindikatoren.

msg("\n### TEIL 9: KONFUNDIERUNG ###\n")

if ("guidance_flag" %in% names(d)) {
  msg("Echte Guidance-Spalte gefunden -- wird verwendet.")
  d[, konf := as.integer(guidance_flag)]
} else {
  msg("Keine Guidance-Spalte im Panel. Es werden Ersatzindikatoren benutzt.")
}

# --- 9.1 Umsatz bestaetigt den Gewinn ---------------------------------------
# Eine Gewinnueberraschung, die vom Umsatz getragen wird, ist persistenter als
# eine, die aus einer Kostenposition stammt. Wenn der Drift nur bei
# bestaetigten Ueberraschungen auftritt, ist das ein GUTES Zeichen: es passt
# zum Mechanismus aus Teil 5.
k91 <- d[!is.na(sue_q5), .(n = .N, median_car = median(car90)),
         by = .(sue_q5, rev_bestaetigt)]
print(dcast(k91, sue_q5 ~ rev_bestaetigt, value.var = "median_car"))
P(ggplot(k91, aes(factor(sue_q5), median_car, fill = rev_bestaetigt)) +
    geom_col(position = "dodge") + geom_hline(yintercept = 0) +
    labs(title = "Median-CAR90: bestaetigt der Umsatz die Gewinnueberraschung?",
         x = "SUE-Quintil", y = "Median CAR90", fill = "Umsatz gleiche Richtung"),
  "26_konf_umsatz")

# --- 9.2 Gedraengter Berichtstag --------------------------------------------
# An vollen Berichtstagen ist die Aufmerksamkeit knapp. Ist der Drift dort
# groesser, misst du einen Aufmerksamkeitseffekt. Das ist ein echter Befund,
# aber ein anderer als der klassische PEAD.
d[, crowd_gr := fifelse(crowd_n >= median(crowd_n, na.rm = TRUE),
                        "voller Tag", "ruhiger Tag")]
k92 <- d[!is.na(sue_q5), .(n = .N, median_car = median(car90)),
         by = .(sue_q5, crowd_gr)]
P(ggplot(k92, aes(factor(sue_q5), median_car, fill = crowd_gr)) +
    geom_col(position = "dodge") + geom_hline(yintercept = 0) +
    labs(title = "Median-CAR90 nach Gedraenge am Berichtstag",
         subtitle = "Groesserer Drift an vollen Tagen = Aufmerksamkeitseffekt",
         x = "SUE-Quintil", y = "Median CAR90", fill = NULL),
  "27_konf_gedraenge")

# --- 9.3 Analystenrevision im selben Quartal --------------------------------
# rec_mean: 1 = Strong Buy, 5 = Sell. Eine NEGATIVE Veraenderung ist also eine
# Hochstufung. Das ist der naechstbeste Ersatz fuer eine Prognoseanhebung.
d[, rec_gr := fifelse(is.na(rec_chg), "unbekannt",
              fifelse(rec_chg < -0.05, "hochgestuft",
              fifelse(rec_chg >  0.05, "abgestuft", "unveraendert")))]
k93 <- d[!is.na(sue_q5) & rec_gr != "unbekannt",
         .(n = .N, median_car = median(car90)), by = .(sue_q5, rec_gr)]
print(dcast(k93, sue_q5 ~ rec_gr, value.var = "median_car"))
P(ggplot(k93, aes(factor(sue_q5), median_car, fill = rec_gr)) +
    geom_col(position = "dodge") + geom_hline(yintercept = 0) +
    labs(title = "Median-CAR90 nach Richtung der Analystenrevision",
         x = "SUE-Quintil", y = "Median CAR90", fill = NULL),
  "28_konf_revision")

# ERGEBNIS? Wenn die Spanne zwischen Q5 und Q1 innerhalb der Gruppe
# "unveraendert" verschwindet und nur bei "hochgestuft" besteht, trennst du
# nicht nach Ueberraschung, sondern nach Analystenverhalten. Dann ist dein
# Signal ein Revisionssignal -- was auch in Ordnung ist, nur muss es dann
# gegen die Revisionsliteratur begruendet werden und nicht gegen PEAD.


