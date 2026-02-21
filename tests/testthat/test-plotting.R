# plot_misch_verlauf() Tests ####

## Teste Klasse möglicher Rückgaben ####
### Summenplots ####
test_that("plot_misch_verlauf (summe/barplot) liefert ggplot", {
  test <- plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "URT010", plot_typ = "barplot")
  expect_s3_class(test, class = "ggplot")
})

test_that("plot_misch_verlauf (summe/treppen) liefert ggplot", {
  # URT010 enthält keine überlappenden Intervalle, MUS001 schon
  test <- plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "URT010", plot_typ = "treppen")
  expect_s3_class(test, class = "ggplot")
})

test_that("plot_misch_verlauf (summe/kombiniert) liefert ggplot", {
  test <- plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "URT010", plot_typ = "kombiniert")
  expect_s3_class(test, class = "ggplot")
})

### Summenplots gruppiert ####

test_that("plot_misch_verlauf (summe/kombiniert) liefert ggplot", {
  test <- plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, "URT010",
    plot_typ = "barplot_gruppen",
    zulassungstyp = "Alle", plot_parametergruppe = "PARAMETERGRUPPE"
  )
  expect_s3_class(test, class = "ggplot")
})

### Einzelsubstanzen ####

test_that("plot_misch_verlauf (einzel/striche) liefert ggplot", {
  test <- plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "URT010", plot_typ = "striche", id_substanz = 71)
  expect_s3_class(test, class = "ggplot")
})

test_that("plot_misch_verlauf (einzel/treppen) liefert ggplot", {
  test <- plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "URT010", plot_typ = "treppen", id_substanz = 71)
  expect_s3_class(test, class = "ggplot")
})

test_that("plot_misch_verlauf (einzel/barplot) liefert ggplot", {
  test <- plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "URT010", plot_typ = "barplot", id_substanz = 71)
  expect_s3_class(test, class = "ggplot")
})

### Mehrere Substanzen ####

test_that("plot_misch_verlauf (mehrere/striche) liefert ggplot", {
  test <- plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "URT010", plot_typ = "striche", id_substanz = c(71, 91))
  expect_s3_class(test, class = "ggplot")
})

test_that("plot_misch_verlauf (mehrere/treppen) liefert ggplot", {
  test <- plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "URT010", plot_typ = "treppen", id_substanz = c(71, 91))
  expect_s3_class(test, class = "ggplot")
})

## Snapshot Test ####
test_that("plot_misch_verlauf bsp ist identisch zu snapshot", {
  # Nicht durch Github Actions/CI ausführen, da fehleranfällig
  skip_on_ci()
  test <- plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "MUS001", plot_typ = "barplot")
  vdiffr::expect_doppelganger("plot_misch_verlauf", test)
})

## Stelle Abbruch sicher ####

# NB: Aktuell gibt es in den Testdaten keine Überlappungen mehr, daher Test auskommentiert.

# test_that("plot_misch_verlauf erzeugt fehler bei treppenplot mit überlappenden intervallen", {
#   expect_condition(plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "MUS001", plot_typ = "treppen"), class = "mvwizr_treppen_ueberlapp")
# })

# plot_misch_ue() Tests ####
## Teste Klasse möglicher Rückgaben ####
### Andauernd ####
test_that("plot_misch_ue (andauernd) liefert patchwork", {
  test <- plot_misch_ue(rq_ue_beispiel_mvwizr, stationscode = "URT010", plot_typ = "andauernd", jahr = c(2019, 2020))
  expect_s3_class(test, class = "ggplot")
})

### Kurzzeitig ####
test_that("plot_misch_ue (kurzzeitig) liefert ggplot", {
  test <- plot_misch_ue(rq_ue_beispiel_mvwizr, stationscode = "URT010", plot_typ = "kurzzeitig", jahr = 2019)
  expect_s3_class(test, class = "ggplot")
})

## Snapshot Test ####
test_that("plot_misch_ue beispiel ist identisch zu snapshot", {
  skip_on_ci()
  test <- plot_misch_ue(rq_ue_beispiel_mvwizr, stationscode = "URT010", plot_typ = "andauernd", jahr = 2019)
  vdiffr::expect_doppelganger("plot_misch_ue", test)
})

# plot_misch_ue_summe() Tests ####
## Teste Klasse möglicher Rückgaben ####
### Mischproben ####
test_that("plot_misch_ue_summe liefert ggplot", {
  test <- plot_misch_ue_summe(rq_ue_beispiel_mvwizr)
  expect_s3_class(test, class = "ggplot")
})

## Snapshot Test ####
test_that("plot_misch_ue_summe bsp ist identisch zu snapshot", {
  skip_on_ci()
  test <- plot_misch_ue_summe(rq_ue_beispiel_mvwizr)
  vdiffr::expect_doppelganger("plot_misch_ue_summe", test)
})

# plot_misch_ue_qk() Tests ####
## Teste Klasse möglicher Rückgaben ####
### Chronisch ####
test_that("plot_misch_ue_qk (chronisch) liefert ggplot", {
  test <- plot_misch_ue_qk(rq_ue_beispiel_mvwizr, qk = "chronisch", detailliert = FALSE)
  expect_s3_class(test, class = "ggplot")
})

### Akut ####
test_that("plot_misch_ue_qk (akut) liefert ggplot", {
  test <- plot_misch_ue_qk(rq_ue_beispiel_mvwizr, qk = "akut", detailliert = FALSE)
  expect_s3_class(test, class = "ggplot")
})

### Detaillierter Plot (VSA) - chronisch ####
test_that("plot_misch_ue_qk (chronisch-detailliert) liefert ggplot", {
  test <- plot_misch_ue_qk(rq_ue_beispiel_mvwizr, qk = "chronisch", detailliert = TRUE)
  expect_s3_class(test, class = "ggplot")
})

## Snapshot Tests ####
test_that("plot_misch_ue_qk bsp ist identisch zu snapshot", {
  skip_on_ci()
  test <- plot_misch_ue_qk(rq_ue_beispiel_mvwizr, qk = "chronisch", detailliert = FALSE)
  vdiffr::expect_doppelganger("plot_misch_ue_qk_chron", test)
})

test_that("plot_misch_ue_qk detailliert bsp ist identisch zu snapshot", {
  skip_on_ci()
  test <- plot_misch_ue_qk(rq_ue_beispiel_mvwizr, qk = "chronisch", detailliert = TRUE)
  vdiffr::expect_doppelganger("plot_misch_ue_qk_chron_detail", test)
})

# plot_misch_oekotox_uebersicht() Tests ####

## Teste Klasse möglicher Rückgaben ####

### Kurzzeitig ####
test_that("plot_misch_oekotox_uebersicht (kurzzeitig) liefert patchwork", {
  test <- plot_misch_oekotox_uebersicht(rq_ue_beispiel_mvwizr, stationscode = "MUS001", jahr = 2020, modus = "kurzzeitig")
  expect_s3_class(test, class = "patchwork")
})

### Andauernd ####
test_that("plot_misch_oekotox_uebersicht (andauernd) liefert patchwork", {
  test <- plot_misch_oekotox_uebersicht(rq_ue_beispiel_mvwizr, stationscode = "URT010", jahr = 2020)
  expect_s3_class(test, class = "patchwork")
})

## Snapshot Test ####
test_that("plot_misch_oekotox_uebersicht ist identisch zu snapshot", {
  skip_on_ci()
  test <- plot_misch_oekotox_uebersicht(rq_ue_beispiel_mvwizr, stationscode = "URT010", jahr = 2020)
  vdiffr::expect_doppelganger("plot_misch_oekotox_uebersicht", test)
})

# plot_misch_mixtox_verlauf() Tests ####

## Teste Klasse möglicher Rückgaben ####

### Andauernd ####
test_that("plot_misch_mixtox_verlauf (andauernd) liefert ggplot", {
  test <- plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "andauernd")
  expect_s3_class(test, class = "ggplot")
})

### Andauernd - mit Akkumulation ####
test_that("plot_misch_mixtox_verlauf (andauernd) liefert ggplot", {
  test <- plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "andauernd", optin_mischtox_S = TRUE)
  expect_s3_class(test, class = "ggplot")
})

### Kurzzeitig ####
test_that("plot_misch_mixtox_verlauf (kurzzeitig) liefert ggplot", {
  test <- plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "kurzzeitig")
  expect_s3_class(test, class = "ggplot")
})

### Andauernd - Zusammenfassung ####
test_that("plot_misch_mixtox_verlauf (andauernd zf) liefert ggplot", {
  test <- plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "andauernd", plot_zusammenfassung = "mischproben")
  expect_s3_class(test, class = "ggplot")
})

### Kurzzeitig - Zusammenfassung ####
test_that("plot_misch_mixtox_verlauf (kurzzeitig zf) liefert ggplot", {
  test <- plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "kurzzeitig", plot_zusammenfassung = "mischproben")
  expect_s3_class(test, class = "ggplot")
})

### Stichproben - Zusammenfassung ####
test_that("plot_misch_mixtox_verlauf (stichproben zf) liefert ggplot", {
  test <- plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "kurzzeitig", plot_zusammenfassung = "stichproben")
  expect_s3_class(test, class = "ggplot")
})

## Snapshot Tests ####
test_that("plot_misch_mixtox_verlauf ist identisch zu snapshot", {
  skip_on_ci()
  test <- plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "andauernd")
  vdiffr::expect_doppelganger("plot_misch_mixtox_verlauf", test)
})

test_that("plot_misch_mixtox_verlauf mit Akkumulationist identisch zu snapshot", {
  skip_on_ci()
  test <- plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "andauernd", optin_mischtox_S = TRUE)
  vdiffr::expect_doppelganger("plot_misch_mixtox_verlauf_akkumulation", test)
})

test_that("plot_misch_mixtox_verlauf zf ist identisch zu snapshot", {
  skip_on_ci()
  test <- plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "andauernd", plot_zusammenfassung = "mischproben")
  vdiffr::expect_doppelganger("plot_misch_mixtox_verlauf_zf", test)
})

# plot_misch_mixtox_haeufigkeit() Tests ####

## Teste Klasse möglicher Rückgaben ####

### Andauernd ####
test_that("plot_misch_mixtox_haeufigkeit (andauernd) liefert ggplot", {
  test <- plot_misch_mixtox_haeufigkeit(rq_ue_beispiel_mvwizr, stationscode = "URT010", modus = "andauernd")
  expect_s3_class(test, class = "ggplot")
})

### Kurzzeitig ####
test_that("plot_misch_mixtox_haeufigkeit (kurzzeitig) liefert ggplot", {
  test <- plot_misch_mixtox_haeufigkeit(rq_ue_beispiel_mvwizr, stationscode = "URT010", modus = "kurzzeitig")
  expect_s3_class(test, class = "ggplot")
})

## Snapshot Test ####
test_that("plot_misch_mixtox_haeufigkeit ist identisch zu snapshot", {
  skip_on_ci()
  test <- plot_misch_mixtox_haeufigkeit(rq_ue_beispiel_mvwizr, stationscode = "URT010", modus = "andauernd")
  vdiffr::expect_doppelganger("plot_misch_mixtox_haeufigkeit", test)
})

# plot_stich_uebersicht() Tests ####

## Teste Klasse möglicher Rückgaben ####
test_that("plot_stich_uebersicht liefert ggplot", {
  test <- plot_stich_uebersicht(mvdaten_beispiel_mvwizr, stationscode = "SA51")
  expect_s3_class(test, class = "ggplot")
})

## Snapshot Test ####
test_that("plot_stich_uebersicht ist identisch zu snapshot", {
  skip_on_ci()
  test <- plot_stich_uebersicht(mvdaten_beispiel_mvwizr, stationscode = "SA51")
  vdiffr::expect_doppelganger("plot_stich_uebersicht", test)
})

# plot_stich_verlauf() Tests ####

## Teste Klasse möglicher Rückgaben ####
test_that("plot_stich_verlauf liefert ggplot", {
  test <- plot_stich_verlauf(mvdaten_beispiel_mvwizr)
  expect_s3_class(test, class = "ggplot")
})

# Fehlerbedingungen Tests ####

## plot_misch_verlauf() Fehler ####

test_that("plot_misch_verlauf stoppt bei nicht-existenter Station", {
  expect_error(
    plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "INEXISTENT"),
    class = "mvwizr_error_empty_dataset"
  )
})

test_that("plot_misch_verlauf stoppt bei ungültigem plot_typ", {
  expect_error(
    plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, stationscode = "URT010", plot_typ = "ungueltig")
  )
})

## plot_misch_ue() Fehler ####

test_that("plot_misch_ue stoppt bei nicht-existenter Station", {
  expect_error(
    plot_misch_ue(rq_ue_beispiel_mvwizr, stationscode = "INEXISTENT", plot_typ = "andauernd", jahr = 2019),
    class = "mvwizr_error_empty_dataset"
  )
})

## plot_misch_mixtox_haeufigkeit() Fehler ####

test_that("plot_misch_mixtox_haeufigkeit stoppt bei nicht-existenter Station", {
  expect_error(
    plot_misch_mixtox_haeufigkeit(rq_ue_beispiel_mvwizr, stationscode = "INEXISTENT", modus = "andauernd"),
    class = "mvwizr_error_empty_dataset"
  )
})

## plot_stich_uebersicht() Fehler ####

test_that("plot_stich_uebersicht stoppt bei Station ohne Stichproben", {
  expect_error(
    plot_stich_uebersicht(mvdaten_beispiel_mvwizr, stationscode = "INEXISTENT"),
    class = "mvwizr_error_empty_dataset"
  )
})
