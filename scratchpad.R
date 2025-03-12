# Beispielscript mit allen Plotfunktionen
library(tidyverse)
library(mvwizr)
if (!dir.exists("output")) dir.create("output")
# Mischproben plotten ####

## Konzentrationen ####

# Summenplot mit Barplot-Darstellung
plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "URT010", plot_typ = "barplot")
ggsave("output/plot_misch_verlauf-barplot.png", width = 20, height = 6)

# Summenplot mit Treppen-Darstellung
plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "URT010", plot_typ = "treppen")
ggsave("output/plot_misch_verlauf-treppen.png", width = 20, height = 6)

# Verlauf von Einzelsubstanzen mit Barplots und Anzeige der Bestimmungsgrenze:
plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "URT010", plot_typ = "barplot", id_substanz = 71)
ggsave("output/plot_misch_verlauf-barplot_ID71.png", width = 20, height = 6)

# Verlauf mehrerer Substanzen mit Strichen:
plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "URT010", plot_typ = "striche", id_substanz = c(71, 91))
ggsave("output/plot_misch_verlauf-striche-ID71-ID91.png", width = 20, height = 6)

# Verlauf Summe aller Substanzen (nicht nur Pestizide) als gruppierte Barplots
plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, "URT010", plot_typ = "barplot_gruppen",
                   zulassungstyp = "Alle", plot_parametergruppe = "PARAMETERGRUPPE")
ggsave("output/plot_misch_verlauf-barplot_gruppen.png", width = 20, height = 6)

## Ue GSchV ####

# Andauernde Überschreitungen (und Unterschreitungen) für die Jahre 2019 und 2020 plotten
plot_misch_ue(rq_ue_beispiel_mvwizr, stationscode = "URT010", plot_typ = "andauernd", jahr = c(2019, 2020))
ggsave("output/plot_misch_ue-andauernd-2019-2020.png", width = 12, height = 9)

# Kurzzeitige Überschreitungen für 2019 anzeigen
plot_misch_ue(rq_ue_beispiel_mvwizr, stationscode = "URT010", plot_typ = "kurzzeitig", jahr = 2019)
ggsave("output/plot_misch_ue-kurzzeitig_2019.png", width = 12, height = 9)

# Kurzzeitige Überschreitungen für Station MUS001 für gesamte Zeitdauer in Daten anzeigen
plot_misch_ue(rq_ue_beispiel_mvwizr, stationscode = "MUS001", plot_typ = "kurzzeitig")
ggsave("output/plot_misch_ue-kurzzeitig_MUS001.png", width = 12, height = 9)

plot_misch_ue_summe(rq_ue_beispiel_mvwizr)
ggsave("output/plot_misch_ue_summe.png", width = 8, height = 6)

plot_misch_ue_qk(rq_ue_beispiel_mvwizr, qk = "chronisch", detailliert = FALSE)
ggsave("output/plot_misch_ue_qk-chronisch.png", width = 8, height = 6)

plot_misch_ue_qk(rq_ue_beispiel_mvwizr, qk = "akut", detailliert = FALSE)
ggsave("output/plot_misch_ue_qk-akut.png", width = 8, height = 6)

pobj_vsa_plot <- plot_misch_ue_qk(rq_ue_beispiel_mvwizr, qk = "chronisch", detailliert = TRUE)
ggsave("output/plot_misch_ue_qk-chronisch-detailliert.png", pobj_vsa_plot, width = 8, height = 6, scale = 1.5)

## Ökotox ####

plot_misch_oekotox_uebersicht(rq_ue_beispiel_mvwizr, stationscode = "URT010", jahr = 2020)
ggsave("output/plot_misch_oekotox_uebersicht-andauernd-2020.png", width = 8, height = 10)

plot_misch_oekotox_uebersicht(rq_ue_beispiel_mvwizr, stationscode = "MUS001", jahr = 2020, modus = "kurzzeitig")
ggsave("output/plot_misch_oekotox_uebersicht-kurzzeitig-2020-MUS001.png", width = 8, height = 10)

plot_misch_oekotox_uebersicht(rq_ue_beispiel_mvwizr, stationscode = "MUS001", jahr = 2020)
ggsave("output/plot_misch_oekotox_uebersicht-andauernd-2020-MUS001.png", width = 8, height = 10)

plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "andauernd")
ggsave("output/plot_misch_mixtox-andauernd.png", width = 12, height = 2)

plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "kurzzeitig")
ggsave("output/plot_misch_mixtox-kurzzeitig.png", width = 12, height = 2)

plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "andauernd", plot_zusammenfassung = "mischproben")
ggsave("output/plot_misch_mixtox-andauernd-zusammenfassung.png", width = 8, height = 6)

plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "kurzzeitig", plot_zusammenfassung = "mischproben")
ggsave("output/plot_misch_mixtox-kurzzeitig-zusammenfassung.png", width = 8, height = 6)

plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "kurzzeitig", plot_zusammenfassung = "stichproben")
ggsave("output/plot_misch_mixtox-stichproben-zusammenfassung.png", width = 8, height = 6)

plot_misch_mixtox_haeufigkeit(rq_ue_beispiel_mvwizr, stationscode = "URT010", modus = "andauernd")
ggsave("output/plot_misch_mixtox_haeufigkeit-andauernd-URT010.png", width = 8, height = 6)

plot_misch_mixtox_haeufigkeit(rq_ue_beispiel_mvwizr, stationscode = "URT010", modus = "kurzzeitig")
ggsave("output/plot_misch_mixtox_haeufigkeit-kurzzeitig-URT010.png", width = 8, height = 6)

# Stichproben plotten ####

plot_stich_uebersicht(mvdaten_beispiel_mvwizr, stationscode = "SA51")
ggsave("output/plot_stich_uebersicht-SA51.png", width = 8, height = 12)

plot_stich_uebersicht(mvdaten_beispiel_mvwizr, stationscode = "KI52")
ggsave("output/plot_stich_uebersicht-KI52.png", width = 8, height = 12)

plot_stich_verlauf(mvdaten_beispiel_mvwizr)
ggsave("output/plot_stich_verlauf.png", width = 8, height = 12)
