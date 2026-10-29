# =============================================================================
# 04_auswertung.R  --  Hat die Gewinnueberraschung 90 Tage Vorhersagekraft?
# =============================================================================
#
# Der Kern der Auswertung, in fuenf Teilen:
#   1. CAR-Pfad je SUE-Quintil      -- das Bild, an dem man den Drift sieht
#   2. Spanne und Long-only-Sicht   -- wie oft funktioniert es, Quartal fuer Quartal
#   3. Ehrliche Standardfehler      -- ist es mehr als Zufall?
#   4. Kosten gegen Effekt          -- bleibt nach Spread und Gebuehren etwas uebrig?
#   5. Zusammenfassung
#
# CAR90 = kumulierte Abnormalrendite vom Einstieg (Schlusskurs Tag +1) bis zum
# Ausstieg 90 Kalendertage spaeter, gemessen gegen den gleichgewichteten Markt
# des Heimatlands.
#
# Weitere Diagnosen (Datenintegritaet, fette Raender, Autokorrelation der
# Ueberraschungen, Konfundierung) stehen in 05_diagnose_optional.R.
#
# Voraussetzung: 01, 02 und 03 sind gelaufen.
# =============================================================================

source("00_config.R")

suppressPackageStartupMessages({
  library(ggplot2)
  library(sandwich)   # geclusterte Kovarianzmatrizen
  library(lmtest)     # coeftest()
})

PLOT_DIR <- "auswertung"

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

# --- Grafik 0: Ereignisse je Quartal ----------------------------------------
# THEORIE: PEAD-Studien mit US-Daten arbeiten mit hunderttausenden Ereignissen.
# Du hast ein paar zehntausend, verteilt auf rund 22 Quartale (SUE braucht
# sechs Vorquartale, die Historie beginnt 2019). Die Zahl der
# QUARTALE ist dabei die bindende Groesse, nicht die Zahl der Ereignisse --
# denn Ereignisse desselben Quartals teilen sich denselben Marktverlauf und
# zaehlen statistisch nicht als unabhaengige Beobachtungen (siehe Teil 8).
#
# ERGEBNIS? Gleichmaessige Balken sind gut. Ein Einbruch am aktuellen Rand ist
# normal (das Driftfenster ist noch nicht abgelaufen). Ein Einbruch in der
# Mitte deutet auf ein Datenproblem.
pl <- ev[, .(alle = .N,
             auswertbar = sum(!is.na(car90) & !is.na(sue_rank))), by = quartal]
pl <- melt(pl, id.vars = "quartal")
P(ggplot(pl, aes(quartal, value, fill = variable)) +
    geom_col(position = "dodge") +
    labs(title = "Ereignisse je Quartal",
         subtitle = "auswertbar = CAR90 und SUE-Rang vorhanden",
         x = NULL, y = "Anzahl", fill = NULL) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)),
  "00_events_je_quartal")


# =============================================================================
# TEIL 1 -- CAR-PFAD IN EREIGNISZEIT
# =============================================================================
# THEORIE: Das ist die klassische Abbildung jeder PEAD-Arbeit und der beste
# einzelne Test, den du hast. Auf der x-Achse Handelstage relativ zum Bericht,
# auf der y-Achse die kumulierte Abnormalrendite, eine Linie je Quintil.
#
# WIE EIN SAUBERES ERGEBNIS AUSSIEHT:
#   - Vor Tag 0 liegen alle Linien nahe beieinander (kein Vorlauf, kein Leck)
#   - An Tag 0 faechern sie auf (die Reaktion auf die Nachricht)
#   - Danach laufen sie WEITER auseinander, monoton und ohne Sprung
#
# WAS EIN PROBLEM ANZEIGT:
#   - Auffaechern schon VOR Tag 0: entweder ist dein Berichtsdatum falsch
#     (dann misst du die Reaktion im Vorlauffenster), oder die Information war
#     vorher am Markt. Beides macht den spaeteren "Drift" wertlos.
#   - Ein Sprung an Tag 0 und danach flach: keine Drift, sondern eine
#     effiziente Reaktion. Das ist das Ergebnis, das die neuere US-Literatur
#     fuer grosse Titel berichtet.
#   - Linien, die nicht monoton nach Quintil sortiert sind: das Signal
#     trennt nicht.

msg("\n### TEIL 1: CAR-PFAD ###\n")

pfad <- merge(et[k <= 66L], d[, .(eid, sue_q5, liq_dec, pre_event_vol)], by = "eid")

# Median statt Mittelwert: Renditen haben fette Raender, einzelne Ausreisser
# tragen sonst den Mittelwert (Details: Teil 2 in 05_diagnose_optional.R).
pf_agg <- pfad[!is.na(sue_q5), .(car = median(car_t, na.rm = TRUE), n = .N),
               by = .(k, sue_q5)]

P(ggplot(pf_agg, aes(k, car, colour = factor(sue_q5))) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_hline(yintercept = 0) +
    geom_line(linewidth = 0.9) +
    labs(title = "Median-CAR in Ereigniszeit je SUE-Quintil",
         subtitle = "Tag 0 = erster Handelstag nach dem Bericht | Einstieg Tag +1, Ausstieg nach 90 Kalendertagen (ca. Tag +63)",
         x = "Handelstage relativ zum Bericht",
         y = "kumulierte Abnormalrendite (Median)", colour = "SUE-Quintil"),
  "01_car_pfad_sue")

# Dieselbe Grafik volatilitaetsnormiert -- so faellt der Beitrag der
# schwankungsstarken Titel nicht ins Gewicht.
pfad[, car_std := car_t / pre_event_vol]
P(ggplot(pfad[!is.na(sue_q5) & is.finite(car_std),
              .(car = median(car_std)), by = .(k, sue_q5)],
         aes(k, car, colour = factor(sue_q5))) +
    geom_vline(xintercept = 0, linetype = 2) + geom_hline(yintercept = 0) +
    geom_line(linewidth = 0.9) +
    labs(title = "Median-CAR in Ereigniszeit, volatilitaetsnormiert",
         x = "Handelstage relativ zum Bericht",
         y = "CAR / Vor-Ereignis-Volatilitaet", colour = "SUE-Quintil"),
  "01b_car_pfad_std")

# Und derselbe Pfad fuer das vorwaertsgerichtete Signal (SmartEstimate).
# Erinnerung: pred_surprise misst NICHT die Reaktion auf eine erfolgte
# Ueberraschung, sondern die Erwartung einer kommenden. Wenn hier ein Muster
# entsteht und bei SUE nicht, testest du ein Revisionssignal und keinen PEAD.
pfad2 <- merge(et[k <= 66L], d[!is.na(ps_q5), .(eid, ps_q5)], by = "eid")
P(ggplot(pfad2[, .(car = median(car_t, na.rm = TRUE)), by = .(k, ps_q5)],
         aes(k, car, colour = factor(ps_q5))) +
    geom_vline(xintercept = 0, linetype = 2) + geom_hline(yintercept = 0) +
    geom_line(linewidth = 0.9) +
    labs(title = "Median-CAR je Quintil des Predicted Surprise",
         subtitle = "SmartEstimate minus Konsens -- vorwaertsgerichtet",
         x = "Handelstage relativ zum Bericht", y = "Median CAR",
         colour = "PS-Quintil"),
  "01c_car_pfad_predsurprise")

# Aufgefaechert nach Liquiditaet: existiert der Drift ueberall oder nur unten?
pf_liq <- pfad[!is.na(liq_dec) & !is.na(sue_q5) & sue_q5 %in% c(1, 5),
               .(car = median(car_t, na.rm = TRUE)),
               by = .(k, sue_q5, gruppe = fifelse(liq_dec <= 3, "illiquide (Dez 1-3)",
                                    fifelse(liq_dec >= 8, "liquide (Dez 8-10)", "Mitte")))]
P(ggplot(pf_liq[gruppe != "Mitte"], aes(k, car, colour = factor(sue_q5))) +
    geom_vline(xintercept = 0, linetype = 2) + geom_hline(yintercept = 0) +
    geom_line(linewidth = 0.9) + facet_wrap(~gruppe) +
    labs(title = "CAR-Pfad: liquide gegen illiquide Ereignisse",
         subtitle = "Existiert die Spanne nur links, ist der Effekt nicht handelbar",
         x = "Handelstage relativ zum Bericht", y = "Median CAR",
         colour = "SUE-Quintil"),
  "01d_car_pfad_liquiditaet")


# =============================================================================
# TEIL 2 -- SPANNE JE QUARTAL UND LONG-ONLY-SICHT
# =============================================================================
# THEORIE: Ein einzelner Durchschnitt ueber alle Jahre kann von zwei, drei
# guten Quartalen getragen werden. Wir schauen deshalb Quartal fuer Quartal.

msg("\n### TEIL 2: SPANNE UND LONG-ONLY ###\n")

# Der eigentliche Effekt ist die SPANNE zwischen Top- und Bottom-Quintil.
# Nur sie ist marktneutral -- ein steigendes Niveau in allen Quintilen ist
# schlicht ein guter Markt.
spread_q <- d[sue_q5 %in% c(1, 5), .(m = median(car90)), by = .(quartal, sue_q5)]
spread_q <- dcast(spread_q, quartal ~ sue_q5, value.var = "m")
setnames(spread_q, c("quartal", "q1", "q5"))
spread_q[, spanne := q5 - q1]
spread_q[, idx := seq_len(.N)]

P(ggplot(spread_q, aes(idx, spanne)) +
    geom_col(fill = "grey40") +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "firebrick") +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = spread_q$idx, labels = spread_q$quartal) +
    labs(title = "Long-Short-Spanne je Quartal: Median-CAR90 (Q5 minus Q1)",
         subtitle = "Fallende Gerade = der Effekt verliert ueber die Zeit an Kraft",
         x = NULL, y = "Spanne") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)),
  "02_spanne_je_quartal")

msg("Spanne Q5-Q1, Median ueber alle Quartale: ",
    round(median(spread_q$spanne, na.rm = TRUE), 4))
msg("Anteil Quartale mit positiver Spanne: ",
    round(mean(spread_q$spanne > 0, na.rm = TRUE), 2))

# ERGEBNIS? Der Anteil positiver Quartale ist aussagekraeftiger als der
# Mittelwert der Spanne. Wie oft kaeme dieser Anteil bei einem Muenzwurf
# heraus? Genau das beantwortet der Binomialtest:
bt_spanne <- binom.test(sum(spread_q$spanne > 0, na.rm = TRUE),
                        sum(!is.na(spread_q$spanne)))
msg("Binomialtest 'Spanne > 0' gegen Muenzwurf: p = ",
    signif(bt_spanne$p.value, 2))

# --- Long-only: was davon kannst DU einsammeln? -----------------------------
# Die Spanne Q5 minus Q1 setzt voraus, dass man Q1 leerverkauft. Mit einer
# 90-Tage-Haltepflicht und Einzelaktien ist realistisch nur die Kaufseite.
# Die relevante Frage ist dann: schlaegt Q5 den eigenen Markt?
lo_q <- d[sue_q5 == 5, .(car_q5 = median(car90), n = .N), by = quartal][order(quartal)]
bt_lo <- binom.test(sum(lo_q$car_q5 > 0), nrow(lo_q))
msg("Q5 allein: Median-CAR90 ueber alle Ereignisse ",
    round(d[sue_q5 == 5, median(car90)], 4),
    " | positiv in ", sum(lo_q$car_q5 > 0), " von ", nrow(lo_q),
    " Quartalen (Binomialtest p = ", signif(bt_lo$p.value, 2), ")")

P(ggplot(lo_q, aes(quartal, car_q5)) +
    geom_col(fill = "grey40") + geom_hline(yintercept = 0) +
    labs(title = "Long-only: Median-CAR90 des Top-Quintils je Quartal",
         subtitle = "ueber null = die Kaufseite allein schlaegt den Markt",
         x = NULL, y = "Median CAR90") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)),
  "03_long_only_q5")




# =============================================================================
# TEIL 3 -- GECLUSTERTE STANDARDFEHLER
# =============================================================================
# THEORIE: Der t-Test unterstellt unabhaengige Beobachtungen. Deine Ereignisse
# sind das nicht. Alle Titel, die im Februar 2022 berichten, halten danach
# gemeinsam denselben Zinsschock aus. Ihre Abnormalrenditen sind
# korreliert -- die Marktbereinigung nimmt nur den Durchschnitt heraus, nicht
# die gemeinsame Bewegung innerhalb der Gruppe.
#
# Die Folge ist keine Kleinigkeit. Der naive Standardfehler faellt mit der
# Wurzel aus 20.000 Ereignissen. Der ehrliche faellt mit der Wurzel aus rund
# 22 Quartalen. Der Unterschied ist ein Faktor von etwa 30 --
# t-Werte von 8 koennen so auf unter 2 zusammenfallen.
#
# Faustregel: Cluster auf der Ebene, auf der die Schocks passieren. Hier ist
# das der Zeitpunkt, also das Quartal. Zusaetzlich clustern wir nach Branche,
# weil ein Halbleiterzyklus alle Halbleiter gleichzeitig trifft.
#
# WARNUNG zur Zahl der Cluster: geclusterte Standardfehler sind asymptotisch
# in der Zahl der CLUSTER, nicht der Beobachtungen. Bei rund 22 Quartalen ist das
# wenig. Die uebliche Untergrenze liegt bei etwa 30 bis 50. Deine t-Werte sind
# also selbst nach der Korrektur eher zu optimistisch als zu vorsichtig.

msg("\n### TEIL 3: GECLUSTERTE STANDARDFEHLER ###\n")

reg <- d[!is.na(sue_rank) & !is.na(car90) & !is.na(quartal) & !is.na(industry)]
reg <- as.data.frame(reg[, .(car90, car90_std, sue_rank, ps_rank, car_ann,
                             quartal, industry, liq_dec, jahr)])
reg <- reg[complete.cases(reg[, c("car90", "sue_rank", "quartal", "industry")]), ]
msg("Regressionssample: ", nrow(reg), " Ereignisse, ",
    length(unique(reg$quartal)), " Quartale, ",
    length(unique(reg$industry)), " Branchen.")

fit <- lm(car90 ~ sue_rank, data = reg)

msg("\n(1) naiv, ohne jede Korrektur:")
print(coeftest(fit))
msg("\n(2) heteroskedastie-robust (HC1) -- korrigiert ungleiche Streuung,",
    " aber KEINE Korrelation zwischen Ereignissen:")
print(coeftest(fit, vcov = vcovHC(fit, type = "HC1")))
msg("\n(3) geclustert nach QUARTAL -- das ist die relevante Zahl:")
print(coeftest(fit, vcov = vcovCL(fit, cluster = reg$quartal)))
msg("\n(4) zweifach geclustert, Quartal und Branche:")
print(coeftest(fit, vcov = vcovCL(fit, cluster = reg[, c("quartal", "industry")])))

# --- 8.1 Wie stark faellt der t-Wert? ---------------------------------------
tv <- c(
  naiv     = coeftest(fit)[2, 3],
  hc1      = coeftest(fit, vcov = vcovHC(fit, type = "HC1"))[2, 3],
  quartal  = coeftest(fit, vcov = vcovCL(fit, cluster = reg$quartal))[2, 3],
  zweifach = coeftest(fit, vcov = vcovCL(fit, cluster = reg[, c("quartal", "industry")]))[2, 3]
)
print(round(tv, 2))
P(ggplot(data.table(methode = factor(names(tv), levels = names(tv)),
                    t = as.numeric(tv)),
         aes(methode, abs(t))) +
    geom_col(fill = "grey40") +
    geom_hline(yintercept = 1.96, colour = "firebrick", linetype = 2) +
    labs(title = "Betrag des t-Werts fuer SUE-Rang, nach Korrekturmethode",
         subtitle = "rote Linie = 1,96",
         x = NULL, y = "|t|"),
  "04_t_werte_vergleich")

# --- 8.2 Dasselbe fuer die volatilitaetsnormierte Zielgroesse ---------------
reg2 <- reg[is.finite(reg$car90_std), ]
fit2 <- lm(car90_std ~ sue_rank, data = reg2)
msg("\nZielgroesse car90_std (volatilitaetsnormiert), geclustert nach Quartal:")
print(coeftest(fit2, vcov = vcovCL(fit2, cluster = reg2$quartal)))

# --- 8.3 Kontrolle fuer die Ankuendigungsreaktion ---------------------------
# THEORIE: Wenn SUE nur deshalb den spaeteren Drift erklaert, weil es die
# Reaktion am Berichtstag erklaert, und der Drift blosse Fortsetzung dieser
# Reaktion ist, dann verschwindet der SUE-Koeffizient, sobald car_ann
# kontrolliert wird. Ueberlebt er, hat SUE Information ueber car_ann hinaus.
reg3 <- reg[is.finite(reg$car_ann), ]
fit3 <- lm(car90 ~ sue_rank + car_ann, data = reg3)
msg("\nMit Kontrolle fuer die Ankuendigungsreaktion, geclustert nach Quartal:")
print(coeftest(fit3, vcov = vcovCL(fit3, cluster = reg3$quartal)))

# --- 8.4 Fama-MacBeth als Gegenprobe ----------------------------------------
# Eine zweite, voellig andere Art, dasselbe Problem zu loesen: erst je Quartal
# eine eigene Regression, dann den Mittelwert der Koeffizienten ueber die
# Quartale testen. Die Zahl der Beobachtungen im t-Test ist dann per
# Konstruktion die Zahl der Quartale. Wenn (3) und diese Rechnung weit
# auseinanderliegen, trau keiner von beiden.
fm <- d[!is.na(sue_rank) & !is.na(car90), {
  if (.N < 30) NULL else as.list(coef(lm(car90 ~ sue_rank)))
}, by = quartal]
setnames(fm, c("quartal", "alpha", "beta"))
msg("\nFama-MacBeth ueber ", nrow(fm), " Quartale:")
msg("  mittleres Beta: ", round(mean(fm$beta), 4),
    " | t-Wert: ", round(mean(fm$beta) / (sd(fm$beta) / sqrt(nrow(fm))), 2),
    " | Anteil Quartale mit Beta > 0: ", round(mean(fm$beta > 0), 2))

P(ggplot(fm, aes(seq_len(nrow(fm)), beta)) +
    geom_col(fill = "grey40") + geom_hline(yintercept = 0) +
    geom_hline(yintercept = mean(fm$beta), colour = "firebrick") +
    scale_x_continuous(breaks = seq_len(nrow(fm)), labels = fm$quartal) +
    labs(title = "Quartalsweise Steigung von CAR90 auf den SUE-Rang",
         subtitle = "rote Linie = Mittelwert ueber alle Quartale",
         x = NULL, y = "Beta") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)),
  "04b_fama_macbeth_beta")


# =============================================================================
# TEIL 4 -- KOSTEN GEGEN EFFEKT, JE LIQUIDITAETSDEZIL
# =============================================================================
# THEORIE: Das ist die Frage, an der die meisten Anomalien sterben. Ein Effekt
# existiert statistisch, ist aber kleiner als die Kosten, ihn einzusammeln.
# Genau das ist die Standarderklaerung dafuer, warum PEAD in illiquiden
# Titeln laenger ueberlebt hat: nicht weil ihn niemand kennt, sondern weil
# ihn niemand guenstig genug handeln kann.
#
# Die Kostenschaetzung hier ist bewusst konservativ zusammengesetzt:
#   Round-Turn = 2 x halber Spread (rein und raus) + 2 x deine Gebuehren
# Market Impact ist NICHT enthalten. Bei den unteren Dezilen wuerde er die
# Rechnung noch einmal deutlich verschlechtern.

msg("\n### TEIL 4: KOSTEN GEGEN EFFEKT ###\n")

kost <- d[!is.na(liq_dec), .(
  n            = .N,
  adv_median   = median(adv_pre, na.rm = TRUE),
  spread_bps   = 1e4 * median(spread_pre, na.rm = TRUE),
  car_q5       = median(car90[sue_q5 == 5], na.rm = TRUE),
  car_q1       = median(car90[sue_q5 == 1], na.rm = TRUE)
), by = liq_dec][order(liq_dec)]

kost[, spanne_bps := 1e4 * (car_q5 - car_q1)]
kost[, kosten_bps := spread_bps + 2 * CFG$cost_bps]   # Spread rein+raus =
                                                      # 2 x halber Spread
kost[, netto_bps  := spanne_bps - kosten_bps]
print(kost[, .(liq_dec, n, adv_mio = round(adv_median / 1e6, 1),
               spread_bps = round(spread_bps), spanne_bps = round(spanne_bps),
               kosten_bps = round(kosten_bps), netto_bps = round(netto_bps))])

P(ggplot(melt(kost[, .(liq_dec, Effekt = spanne_bps, Kosten = kosten_bps)],
              id.vars = "liq_dec"),
         aes(factor(liq_dec), value, fill = variable)) +
    geom_col(position = "dodge") +
    labs(title = "Bruttoeffekt gegen Handelskosten je Liquiditaetsdezil",
         subtitle = "Effekt = Median-CAR90 Q5 minus Q1 | Kosten = Spread-Schaetzer plus Gebuehren, ohne Market Impact",
         x = "Liquiditaetsdezil (1 = illiquideste)", y = "Basispunkte",
         fill = NULL),
  "05_kosten_vs_effekt")

P(ggplot(kost, aes(factor(liq_dec), netto_bps)) +
    geom_col(aes(fill = netto_bps > 0)) +
    geom_hline(yintercept = 0) +
    scale_fill_manual(values = c("firebrick", "seagreen"), guide = "none") +
    labs(title = "Nettoeffekt nach Kosten je Liquiditaetsdezil",
         x = "Liquiditaetsdezil", y = "Basispunkte je Trade"),
  "06_netto_je_dezil")

# ERGEBNIS? Das typische Bild in der Literatur: der Bruttoeffekt faellt mit
# steigender Liquiditaet, die Kosten fallen schneller. Wenn bei dir nur die
# Dezile 1 bis 3 netto positiv sind, hast du keine Strategie, sondern eine
# Messung von Illiquiditaet. Long-only zaehlt fuer dich eher car_q5 allein
# gegen die halben Kosten der Spanne (nur eine Seite wird gehandelt).


# =============================================================================
# TEIL 5 -- ZUSAMMENFASSUNG
# =============================================================================
# Die Zahlen, an denen die Entscheidung haengt, ob sich ein Backtest lohnt.
# Trag sie dir weg und vergleiche sie, wenn du Parameter aenderst -- und
# zaehl dabei mit, wie viele Varianten du probiert hast. Wer zwanzig Varianten
# testet, findet eine mit p < 0,05 auch ohne echten Effekt.

msg("\n#########################################################")
msg("### ZUSAMMENFASSUNG (Haltedauer ", CFG$hold_days, " Tage)")
msg("#########################################################")
msg("1) Auswertbare Ereignisse       : ", nrow(d),
    " in ", uniqueN(d$quartal), " Quartalen")
msg("2) Spanne Q5-Q1 (Median CAR90)  : ",
    round(1e4 * median(spread_q$spanne, na.rm = TRUE)), " bps",
    " | positiv in ", round(100 * mean(spread_q$spanne > 0, na.rm = TRUE)),
    "% der Quartale (p = ", signif(bt_spanne$p.value, 2), ")")
msg("3) Long-only Q5 (Median CAR90)  : ",
    round(1e4 * d[sue_q5 == 5, median(car90)]), " bps",
    " | positiv in ", round(100 * mean(lo_q$car_q5 > 0)),
    "% der Quartale (p = ", signif(bt_lo$p.value, 2), ")")
msg("4) t-Wert SUE-Rang, geclustert  : ", round(tv["quartal"], 2),
    "   (naiv waere es ", round(tv["naiv"], 2), ")",
    " | Fama-MacBeth: ", round(mean(fm$beta) / (sd(fm$beta) / sqrt(nrow(fm))), 2))
msg("5) Dezile mit positivem Netto   : ",
    paste(kost[netto_bps > 0, liq_dec], collapse = ", "),
    "   (von 1 bis 10)")
msg("6) Ereignisse ohne Kurse        : delistete Titel fehlen weitgehend, ",
    "siehe output/price_check.csv -- das Ergebnis ist eher zu optimistisch.")
msg("")
msg("Grafiken: ", dir_plot)
msg("#########################################################")
