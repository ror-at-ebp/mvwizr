# batch_einlesen_nawa() Tests ####

## Teste Struktur der Rückgabe ####

test_that("batch_einlesen_nawa produziert tibble mit korrekten variablen", {
  nawa_mv_pfade <- system.file("extdata", c("NAWA_Bsp_1.xlsx", "NAWA_Bsp_2.xlsx", "NAWA_Bsp_3.csv", "NAWA_Bsp_4.xlsx", "NAWA_Bsp_5.csv"), package = "mvwizr")

  rlang::local_options(rlib_message_verbosity = "quiet")
  rlang::local_options(rlib_warning_verbosity = "quiet")

  # Ohne explizite Angabe von BAFU/VSA-Pfaden - Test ob Standardwert funktioniert
  out <- withr::with_options(
    list(cli.default_handler = function(...) { }),
    batch_einlesen_nawa(nawa_mv_pfade)
  )

  fixe_var <- c(
    "filename",
    "UID", "CODE", "STANDORT", "NAME", "PROBEARTID", "BEGINNPROBENAHME",
    "ENDEPROBENAHME", "ID_Substanz", "PARAMETERID_BAFU", "Messwert",
    "WERT_NUM", "EINHEIT", "BAFU_Bez_DE",
    "BAFU_Bez_FR", "BG_max", "BG_min"
  )

  out_names <- names(out)
  var_vorhanden_bool <- all(fixe_var %in% out_names)

  datum_parsed_bool <- anyNA(out$BEGINNPROBENAHME)

  expect_s3_class(out, class = c("tbl_df", "tbl", "data.frame"))
  expect_true(var_vorhanden_bool)
  expect_false(datum_parsed_bool)
  expect_type(out$ID_Substanz, "integer")
  expect_type(out$WERT_NUM, "double")
  expect_false(anyNA(out$ID_Substanz))
  expect_false(anyNA(out$PARAMETERID_BAFU))
})

# schreibe_nawa_import_manifest_template() Tests ####

## Teste Struktur der Rückgabe und dass Datei existiert ####

test_that("schreibe_nawa_import_manifest_template erzeugt korrektes tibble", {
  nawa_mv_pfade <- system.file("extdata", c("NAWA_Bsp_1.xlsx", "NAWA_Bsp_2.xlsx", "NAWA_Bsp_3.csv", "NAWA_Bsp_4.xlsx", "NAWA_Bsp_5.csv"), package = "mvwizr")
  filepath <- tempfile(fileext = ".xlsx")
  return <- withr::with_options(
    list(cli.default_handler = function(...) { }),
    schreibe_nawa_import_manifest_template(import_manifest_file = filepath, mv_datei_pfade = nawa_mv_pfade)
  )

  fixed_names <- c("file", "encoding", "header", "delimiter", "lang")

  is_tibble_bool <- tibble::is_tibble(return)
  dim_correct_bool <- all(dim(return) == c(5L, 5L))
  names_correct_bool <- all(fixed_names %in% names(return))
  file_exists_bool <- file.exists(filepath)

  expect_true(all(is_tibble_bool, dim_correct_bool, names_correct_bool, file_exists_bool))
})

# einlesen_nawa() Tests ####

## Teste, dass Heuristik funktioniert ####

test_that("einlesen_nawa funktioniert korrekt mit heuristik", {
  nawa_mv_fr <- system.file("extdata", "NAWA_Bsp_4.xlsx", package = "mvwizr")

  out <- withr::with_options(
    list(cli.default_handler = function(...) { }),
    suppressWarnings(einlesen_nawa(nawa_mv_fr))
  )

  # Testen der Einlesefunktion mit Angabe von Argumenten
  out2 <- withr::with_options(
    list(cli.default_handler = function(...) { }),
    suppressWarnings(einlesen_nawa(nawa_mv_fr,
      lang = "FR", parameter = "BAFU_Bez_FR",
      header = 8L
    ))
  )

  expect_identical(out, out2)
})

## Teste, dass Duplikate korrekt behandelt werden ####

test_that("einlesen_nawa entfernt duplikate", {
  nawa_mv_pfad <- system.file("extdata", c("NAWA_Bsp_2.xlsx"), package = "mvwizr")

  rlang::local_options(rlib_message_verbosity = "quiet")

  # cli.default_handler = function (...){} damit cli::cli_alert messages gemuted werden
  f <- function() {
    withr::with_options(
      list(cli.default_handler = function(...) { }),
      suppressWarnings(einlesen_nawa(nawa_mv_pfad, lang = "DE", header = 8L), classes = "mvwizr_warn_vsa_dup_sid")
    )
  }

  # Es wird eine Warnung ausgegeben, dass 130 Duplikate gefunden wurden - wir wollen diese Warnung verifizieren
  expect_warning(out <- f(), "130 Daten")

  out_test1 <- out |> dplyr::filter(.data$STANDORT == "Standort_203", .data$BEGINNPROBENAHME == lubridate::dmy_hms("24.09.2024  10:00:00"), .data$PARAMETERID_BAFU == "Fipronil")
  out_test2 <- out |> dplyr::filter(.data$STANDORT == "Standort_203", .data$BEGINNPROBENAHME == lubridate::dmy_hms("15.02.2022  10:00:00"), .data$PARAMETERID_BAFU == "Fipronil")
  test1_bool <- identical(out_test1$Bestimmungsgrenze, 5e-5) && out_test1$`Gerät/Methode` == "GC-MS/MS" # Sicherstellen, dass richtige Messung ausgewählt wird wenn beide < BG
  test2_bool <- identical(out_test2$WERT_NUM, 0.000205591) && out_test1$`Gerät/Methode` == "GC-MS/MS" # Sicherstellen, dass richtige Messung ausgewählt wird eine Messung < BG und eine > BG

  expect_true(test1_bool)
  expect_true(test2_bool)
})

# einlesen_mv_gbl() Tests ####

## Teste Struktur der Rückgabe ####

test_that("einlesen_mv_gbl produziert tibble mit korrekten variablen", {
  mv_daten_pfad <- system.file("extdata", "Daten_MV_GBL_2019_2020.txt", package = "mvwizr")
  vsa_lookup_pfad <- system.file("extdata", "Tab_Substanzen.txt", package = "mvwizr")
  bafu_filename <- "BAFU_Liste_Parameter_Bezeichnungen_Datenaustausch.xlsx"
  bafu_code_pfad <- system.file("extdata", bafu_filename, package = "mvwizr")

  rlang::local_options(rlib_message_verbosity = "quiet")
  rlang::local_options(rlib_warning_verbosity = "quiet")

  out <- einlesen_mv_gbl(mv_daten_pfad, vsa_lookup_pfad, bafu_code_pfad)

  fixe_var <- c(
    "UID", "CODE", "STANDORT", "NAME", "PROBEARTID", "BEGINNPROBENAHME",
    "ENDEPROBENAHME", "ID_Substanz", "PARAMETERID_BAFU", "OPERATOR",
    "WERT_NUM", "Konz_inkl_BG", "EINHEIT", "MSTLTYP", "PARAMETERGRUPPEID",
    "PARAMETERGRUPPE", "BEZEICHNUNG_BAFU", "BAFU_Bez_DE",
    "BAFU_Bez_FR", "BG_max", "BG_min"
  )

  out_names <- names(out)
  var_vorhanden_bool <- all(fixe_var %in% out_names)

  expect_s3_class(out, class = c("tbl_df", "tbl", "data.frame"))
  expect_true(var_vorhanden_bool)
  expect_type(out$ID_Substanz, "integer")
  expect_type(out$WERT_NUM, "double")
  expect_false(anyNA(out$ID_Substanz))
  expect_false(anyNA(out$PARAMETERID_BAFU))
})

# einheiten_normalisieren() Tests ####

## Teste Struktur der Rückgabe ####

test_that("normalise_units funktioniert", {
  mv <- mvwizr::mvdaten_beispiel_mvwizr

  out <- normalise_units(mv, wert = "WERT_NUM", einheit = "EINHEIT")
  out_names <- names(out)

  var_identisch_bool <- all(out_names == names(mv))

  einheiten_korrekt_bool <- all(out$EINHEIT == "\u00b5g/l")

  expect_s3_class(out, class = c("tbl_df", "tbl", "data.frame"))
  expect_true(var_identisch_bool)
  expect_true(einheiten_korrekt_bool)
  expect_type(out$ID_Substanz, "integer")
  expect_type(out$WERT_NUM, "double")
})

# einlesen_regulierungen() Tests ####

## Teste Struktur der Rückgabe ####

test_that("einlesen_regulierungen produziert tibble", {
  reg_pfad <- system.file("extdata", "anf_substanz_recht.xlsx", package = "mvwizr")
  out <- einlesen_regulierungen(reg_pfad)
  out_names <- names(out)

  fixe_var <- c("ID_Substanz", "Name_reg", "Informationen Recht", "GSCHV")

  var_vorhanden_bool <- all(fixe_var %in% out_names)

  expect_s3_class(out, class = c("tbl_df", "tbl", "data.frame"))
  expect_true(var_vorhanden_bool)
  expect_type(out$ID_Substanz, "integer")
})

# einlesen_kriterien() Tests ####

## Teste Struktur der Rückgabe ####

test_that("einlesen_kriterien produziert tibble", {
  krit_pfad <- system.file("extdata", "Dat_Qual_kriterien.xlsx", package = "mvwizr")

  rlang::local_options(rlib_message_verbosity = "quiet")
  rlang::local_options(rlib_warning_verbosity = "quiet")

  out <- suppressWarnings(einlesen_kriterien(krit_pfad))
  out_names <- names(out)

  fixe_var <- c(
    "ID_Substanz", "P_chron", "I_chron", "V_chron", "P_akut", "I_akut",
    "V_akut", "Robustheit QK", "CQK", "AQK"
  )

  var_vorhanden_bool <- all(fixe_var %in% out_names)

  expect_s3_class(out, class = c("tbl_df", "tbl", "data.frame"))
  expect_true(var_vorhanden_bool)
  expect_type(out$ID_Substanz, "integer")
  expect_type(out$CQK, "double")
  expect_type(out$AQK, "double")
})

# einlesen_vsa_lookup() Tests ####

## Teste Struktur der Rückgabe ####

test_that("einlesen_vsa_lookup produziert tibble", {
  vsa_lookup_pfad <- system.file("extdata", "Tab_Substanzen.txt", package = "mvwizr")

  rlang::local_options(rlib_message_verbosity = "quiet")
  rlang::local_options(rlib_warning_verbosity = "quiet")

  out <- einlesen_vsa_lookup(vsa_lookup_pfad)
  out_names <- names(out)

  fixe_var <- c("ID_Substanz", "Parameter-ID")

  var_vorhanden_bool <- all(fixe_var %in% out_names)

  duplikate_bool <- any(duplicated(out$ID_Substanz))

  expect_s3_class(out, class = c("tbl_df", "tbl", "data.frame"))
  expect_true(var_vorhanden_bool)
  expect_false(duplikate_bool)
  expect_type(out$ID_Substanz, "integer")
})

# einlesen_bafu_lookup() Tests ####

## Teste Struktur der Rückgabe ####

test_that("einlesen_bafu_lookup produziert tibble", {
  bafu_lookup_pfad <- system.file("extdata", "BAFU_Liste_Parameter_Bezeichnungen_Datenaustausch.xlsx", package = "mvwizr")
  out <- einlesen_bafu_lookup(bafu_lookup_pfad)
  out_names <- names(out)

  fixe_var <- c("BAFU_Parameter_ID", "BAFU_Bez_DE", "BAFU_Bez_FR")

  var_vorhanden_bool <- all(fixe_var %in% out_names)

  duplikate_bool <- any(duplicated(out$BAFU_Parameter_ID))

  expect_s3_class(out, class = c("tbl_df", "tbl", "data.frame"))
  expect_true(var_vorhanden_bool)
  expect_false(duplikate_bool)
})

# berechne_rq_ue() Tests ####

## Teste Struktur der Rückgabe ####

test_that("berechne_rq_ue produziert tibble", {
  rlang::local_options(rlib_message_verbosity = "quiet")
  rlang::local_options(rlib_warning_verbosity = "quiet")

  out <- berechne_rq_ue(mvwizr::mvdaten_beispiel_mvwizr,
    mvwizr::regulierungen_mvwizr,
    mvwizr::kriterien_mvwizr,
    robust3 = FALSE
  )

  out_names <- names(out)

  fixe_var <- c(
    "RQ_CQK",
    "RQ_AQK", "RQ_CQK_P", "RQ_CQK_I", "RQ_CQK_V", "RQ_AQK_P", "RQ_AQK_I",
    "RQ_AQK_V", "Beurteilung_CQK", "Beurteilung_AQK", "Ue_anhaltend",
    "Ue_kurzzeitig", "Ue_spezifisch", "Ue_generisch", "Ue_AQK", "Ue_CQK"
  )

  typen_korrekt <- all({
    out |> dplyr::summarise(
      is_num = dplyr::across(dplyr::starts_with("RQ_"), is.numeric),
      is_logi = dplyr::across(dplyr::starts_with("Ue_"), rlang::is_logical),
      is_fct = dplyr::across(dplyr::starts_with("Beurteilung_"), is.factor)
    )
  })

  var_vorhanden_bool <- all(fixe_var %in% out_names)

  fct_levels_out <- levels(out$Beurteilung_CQK)
  fct_levels_fix <- c("sehr gut", "gut", "mässig", "unbefriedigend", "schlecht")

  fct_korrekt <- all(fct_levels_fix %in% fct_levels_out)

  expect_s3_class(out, class = c("tbl_df", "tbl", "data.frame"))
  expect_true(var_vorhanden_bool)
  expect_true(typen_korrekt)
  expect_true(fct_korrekt)
})

# berechne_mixtox() Tests ####

## Teste Struktur der Rückgabe ####

test_that("berechne_mixtox produziert tibble", {
  out <- berechne_mixtox(mvwizr::rq_ue_beispiel_mvwizr)
  out_names <- names(out)

  fixe_var <- c(
    "CODE", "BEGINNPROBENAHME", "ENDEPROBENAHME", "Jahr", "Tage",
    "Ziel", "Kriterium", "RQ", "Ziel_num", "Beurteilung"
  )

  var_vorhanden_bool <- all(fixe_var %in% out_names)

  levels_ziel <- levels(out$Ziel)
  levels_korrekt_bool <- all(c("Vertebraten", "Invertebraten", "Pflanzen") %in% levels_ziel)

  expect_s3_class(out, class = c("tbl_df", "tbl", "data.frame"))
  expect_true(var_vorhanden_bool)
  expect_true(levels_korrekt_bool)
})

# prozessiere_bSaP() Tests ####

## Teste Struktur der Rückgabe ####

test_that("prozessiere_bSaP produziert tibble", {
  mv <- mvwizr::mvdaten_beispiel_mvwizr
  out <- mvwizr::prozessiere_bSaP(mv, bSaP_identifier = "PROBEARTID")

  out_kleinergl <- nrow(out) <= nrow(mv)

  expect_s3_class(out, class = c("tbl_df", "tbl", "data.frame"))
  expect_true(out_kleinergl)
})

test_that("prozessiere_bSaP stoppt bei falschen parametern", {
  mv <- mvwizr::mvdaten_beispiel_mvwizr
  mv[1, "PROBEARTID"] <- "X"

  expect_error(mvwizr::prozessiere_bSaP(mv, bSaP_identifier = "PROBEARTID"), class = "mvwizr_error_bSaP_identifier_ungueltig")
})

## Teste, dass Funktion auch für NAWA-Dateien funktioniert ####

test_that("prozessiere_bSaP funktioniert mit nawa mv dateien", {
  mv_bsap_pfade <- system.file("extdata", c("NAWA_ohne_bSaP_Bsp.xlsx", "NAWA_mit_bSaP_Bsp.csv"), package = "mvwizr")

  out <- withr::with_options(
    list(cli.default_handler = function(...) { }),
    suppressWarnings(batch_einlesen_nawa(mv_bsap_pfade))
  )
  out_dim_correct_bool <- identical(dim(out), c(155L, 28L))

  out <- dplyr::mutate(out, PROBEARTID = dplyr::if_else(.data$filename == "NAWA_mit_bSaP_Bsp.csv", "bSaP", .data$PROBEARTID))

  mv_bsap <- prozessiere_bSaP(out, bSaP_identifier = "PROBEARTID") |> dplyr::mutate(Dauer = difftime(.data$ENDEPROBENAHME, .data$BEGINNPROBENAHME, units = "days"))
  mv_bsap_dim_correct_bool <- identical(dim(mv_bsap), c(59L, 29L))

  mv_bsap_14d <- mv_bsap |> dplyr::filter(.data$Dauer == lubridate::ddays(14))

  mv_bsap_14d_length_correct_bool <- identical(nrow(mv_bsap_14d), 48L)

  expect_true(out_dim_correct_bool)
  expect_true(mv_bsap_dim_correct_bool)
  expect_true(mv_bsap_14d_length_correct_bool)
})

# Fehlerbedingungen Tests ####

## einlesen_nawa() Fehlerbedingungen ####

test_that("einlesen_nawa stoppt bei nicht-unterstütztem Dateiformat", {
  tmp <- tempfile(fileext = ".json")
  writeLines("{}", tmp)
  expect_error(einlesen_nawa(tmp))
})

test_that("einlesen_nawa stoppt bei mehreren Dateipfaden", {
  pfade <- c("datei1.xlsx", "datei2.xlsx")
  expect_error(einlesen_nawa(pfade))
})

## berechne_rq_ue() Fehlerbedingungen ####

test_that("berechne_rq_ue stoppt bei nicht-dataframe regulierungen", {
  mv <- mvwizr::mvdaten_beispiel_mvwizr
  expect_error(
    berechne_rq_ue(mv, regulierungen = "nicht_ein_dataframe", kriterien = mvwizr::kriterien_mvwizr)
  )
})

test_that("berechne_rq_ue stoppt bei nicht-dataframe kriterien", {
  mv <- mvwizr::mvdaten_beispiel_mvwizr
  expect_error(
    berechne_rq_ue(mv, regulierungen = mvwizr::regulierungen_mvwizr, kriterien = list(1, 2, 3))
  )
})

## berechne_stichproben_gbl_aggregiert() Tests ####

test_that("berechne_stichproben_gbl_aggregiert produziert tibble", {
  mv <- mvwizr::mvdaten_beispiel_mvwizr
  out <- berechne_stichproben_gbl_aggregiert(mv)

  expect_s3_class(out, class = c("tbl_df", "tbl", "data.frame"))
  expect_true(all(c("CODE", "Jahr", "PARAMETERGRUPPE", "agg_summe") %in% names(out)))
  expect_type(out$agg_summe, "double")
})

test_that("berechne_stichproben_gbl_aggregiert stoppt ohne stichproben", {
  mv <- mvwizr::mvdaten_beispiel_mvwizr |>
    dplyr::filter(!is.na(.data$ENDEPROBENAHME))
  expect_error(berechne_stichproben_gbl_aggregiert(mv), class = "mvwizr_error_empty_dataset")
})

# Wertkorrektheits-Tests ####

## normalise_units() Korrektheitstest ####

test_that("normalise_units konvertiert ng/l korrekt zu µg/l", {
  testdaten <- tibble::tibble(
    WERT_NUM = c(1000, 500),
    EINHEIT = c("ng/l", "ng/l")
  )
  out <- normalise_units(testdaten, wert = "WERT_NUM", einheit = "EINHEIT")
  expect_equal(out$WERT_NUM, c(1, 0.5))
  expect_true(all(out$EINHEIT == "\u00b5g/l"))
})

test_that("normalise_units konvertiert mg/l korrekt zu µg/l", {
  testdaten <- tibble::tibble(
    WERT_NUM = c(0.001, 0.01),
    EINHEIT = c("mg/l", "mg/l")
  )
  out <- normalise_units(testdaten, wert = "WERT_NUM", einheit = "EINHEIT")
  expect_equal(out$WERT_NUM, c(1, 10))
  expect_true(all(out$EINHEIT == "\u00b5g/l"))
})

test_that("normalise_units lässt unbekannte Einheiten unverändert", {
  testdaten <- tibble::tibble(
    WERT_NUM = c(42),
    EINHEIT = c("mol/l")
  )
  out <- normalise_units(testdaten, wert = "WERT_NUM", einheit = "EINHEIT")
  expect_equal(out$WERT_NUM, 42)
  expect_equal(out$EINHEIT, "mol/l")
})

## berechne_rq_ue() Wertkorrektheit ####

test_that("berechne_rq_ue berechnet korrekte RQ-Werte", {
  rlang::local_options(rlib_message_verbosity = "quiet")
  rlang::local_options(rlib_warning_verbosity = "quiet")

  out <- berechne_rq_ue(mvwizr::mvdaten_beispiel_mvwizr,
    mvwizr::regulierungen_mvwizr,
    mvwizr::kriterien_mvwizr,
    robust3 = FALSE
  )

  # RQ_CQK = WERT_NUM / CQK - prüfen für eine bestimmte Zeile
  test_row <- out |>
    dplyr::filter(!is.na(.data$CQK), .data$WERT_NUM > 0) |>
    dplyr::slice(1)

  expected_rq <- test_row$WERT_NUM / test_row$CQK
  expect_equal(test_row$RQ_CQK, expected_rq)
})

## berechne_mixtox() Wertkorrektheit ####

test_that("berechne_mixtox summiert RQ-Werte korrekt pro Gruppe", {
  rlang::local_options(rlib_message_verbosity = "quiet")
  rlang::local_options(rlib_warning_verbosity = "quiet")

  rq_daten <- berechne_rq_ue(mvwizr::mvdaten_beispiel_mvwizr,
    mvwizr::regulierungen_mvwizr,
    mvwizr::kriterien_mvwizr,
    robust3 = FALSE
  )
  mixtox <- berechne_mixtox(rq_daten)

  # Manuell berechnen für eine bestimmte Probe
  test_probe <- rq_daten |>
    dplyr::filter(!is.na(.data$ENDEPROBENAHME)) |>
    dplyr::slice(1)

  probe_daten <- rq_daten |>
    dplyr::filter(
      .data$CODE == test_probe$CODE,
      .data$BEGINNPROBENAHME == test_probe$BEGINNPROBENAHME,
      .data$ENDEPROBENAHME == test_probe$ENDEPROBENAHME
    )

  expected_mix_p_cqk <- sum(probe_daten$RQ_CQK_P, na.rm = TRUE)
  # Nur vergleichen wenn es nicht-NA Werte gab
  if (any(!is.na(probe_daten$RQ_CQK_P))) {
    actual <- mixtox |>
      dplyr::filter(
        .data$CODE == test_probe$CODE,
        .data$BEGINNPROBENAHME == test_probe$BEGINNPROBENAHME,
        .data$Ziel == "Pflanzen",
        .data$Kriterium == "CQK"
      )
    expect_equal(actual$RQ, expected_mix_p_cqk)
  }
})

# Interne Hilfsfunktionen Tests ####

## entferne_duplikate() Korrektheitstest ####

test_that("entferne_duplikate behält höchsten Messwert", {
  testdaten <- tibble::tibble(
    CODE = c("A", "A"),
    PROBEARTID = c("SaP", "SaP"),
    BEGINNPROBENAHME = rep(as.POSIXct("2020-01-01"), 2),
    ENDEPROBENAHME = rep(as.POSIXct("2020-01-15"), 2),
    PARAMETER = c("Atrazin", "Atrazin"),
    WERT_NUM = c(0.5, 1.0)
  )

  rlang::local_options(rlib_warning_verbosity = "quiet")
  out <- entferne_duplikate(testdaten)
  expect_equal(nrow(out), 1)
  expect_equal(out$WERT_NUM, 1.0)
})
