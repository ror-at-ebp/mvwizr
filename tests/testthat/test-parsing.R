# einlesen_mv_gbl() Tests ####

## Teste Struktur der Rückgabe ####

test_that("einlesen_mv_gbl produziert tibble mit korrekten variablen", {
  mv_daten_pfad <- system.file("extdata", "Daten_MV_GBL_2019_2020.txt", package = "mvwizr")
  vsa_lookup_pfad <- system.file("extdata", "Tab_Substanzen.txt", package = "mvwizr")
  bafu_filename <- "BAFU_Liste_Parameter_Bezeichnungen_Datenaustausch.xlsx"
  bafu_code_pfad <- system.file("extdata", bafu_filename, package = "mvwizr")

  out <- suppressWarnings(einlesen_mv_gbl(mv_daten_pfad, vsa_lookup_pfad, bafu_code_pfad))

  fixe_var <- c("UID", "CODE", "STANDORT", "NAME", "PROBEARTID", "BEGINNPROBENAHME",
                "ENDEPROBENAHME", "ID_Substanz", "PARAMETERID_BAFU", "OPERATOR",
                "WERT_NUM", "Konz_inkl_BG", "EINHEIT", "MSTLTYP", "PARAMETERGRUPPEID",
                "PARAMETERGRUPPE", "BEZEICHNUNG_BAFU", "BAFU_Bez_DE",
                "BAFU_Bez_FR", "BG_max", "BG_min")

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
  reg_pfad <- system.file("extdata", "anf_Substanz_recht.xlsx", package = "mvwizr")
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

test_that("einlesen_regulierungen produziert tibble", {
  krit_pfad <- system.file("extdata", "Dat_Qual_kriterien.xlsx", package = "mvwizr")
  out <- suppressWarnings(einlesen_kriterien(krit_pfad))
  out_names <- names(out)

  fixe_var <- c("ID_Substanz", "P_chron", "I_chron", "V_chron", "P_akut", "I_akut",
                "V_akut", "Robustheit QK", "CQK", "AQK")

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
  out <- suppressWarnings(einlesen_vsa_lookup(vsa_lookup_pfad))
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
  out <- suppressWarnings(einlesen_bafu_lookup(bafu_lookup_pfad))
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

  out <- berechne_rq_ue(mvwizr::mvdaten_beispiel_mvwizr,
                                  mvwizr::regulierungen_mvwizr,
                                  mvwizr::kriterien_mvwizr,
                                  robust3 = FALSE)

  out_names <- names(out)

  fixe_var <- c("RQ_CQK",
                "RQ_AQK", "RQ_CQK_P", "RQ_CQK_I", "RQ_CQK_V", "RQ_AQK_P", "RQ_AQK_I",
                "RQ_AQK_V", "Beurteilung_CQK", "Beurteilung_AQK", "Ue_anhaltend",
                "Ue_kurzzeitig", "Ue_spezifisch", "Ue_generisch", "Ue_AQK", "Ue_CQK")

  typen_korrekt <- all({out |> dplyr::summarise(is_num = dplyr::across(dplyr::starts_with("RQ_"), is.numeric),
                  is_logi = dplyr::across(dplyr::starts_with("Ue_"), rlang::is_logical),
                  is_fct = dplyr::across(dplyr::starts_with("Beurteilung_"), is.factor))
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

  fixe_var <- c("CODE", "BEGINNPROBENAHME", "ENDEPROBENAHME", "Jahr", "Tage",
                "Ziel", "Kriterium", "RQ", "Ziel_num", "Beurteilung")

  var_vorhanden_bool <- all(fixe_var %in% out_names)

  levels_ziel <- levels(out$Ziel)
  levels_korrekt_bool <- all(c("Vertebraten", "Invertebraten", "Pflanzen") %in% levels_ziel)

  expect_s3_class(out, class = c("tbl_df", "tbl", "data.frame"))
  expect_true(var_vorhanden_bool)
  expect_true(levels_korrekt_bool)
})

# entferne_berech_gbl() Tests ####

## Teste Struktur der Rückgabe ####

test_that("entferne_berech_gbl produziert tibble", {
  mv <- mvwizr::mvdaten_beispiel_mvwizr
  out <- mvwizr:::entferne_berech_gbl(mv)
  out_names <- names(out)

  fixe_var <- c("UID", "CODE", "STANDORT", "NAME", "PROBEARTID", "BEGINNPROBENAHME",
                "ENDEPROBENAHME", "ID_Substanz", "PARAMETERID_BAFU", "OPERATOR",
                "WERT_NUM", "Konz_inkl_BG", "EINHEIT", "MSTLTYP", "PARAMETERGRUPPEID",
                "PARAMETERGRUPPE", "PARAMETER_GBL", "BEZEICHNUNG_BAFU", "BAFU_Bez_DE",
                "BAFU_Bez_FR", "BG_max", "BG_min")

  var_enthalten_bool <- all(fixe_var %in% out_names)

  out_kleinergl <- nrow(out) <= nrow(mv)

  expect_s3_class(out, class = c("tbl_df", "tbl", "data.frame"))
  expect_true(var_enthalten_bool)
  expect_true(out_kleinergl)
})
