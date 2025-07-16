## Code um `mvdaten_beispiel_mvwizr` zu erstellen

mv_daten_pfad <- c("inst/extdata/Daten_MV_GBL_2019_2020.txt", "inst/extdata/Stichproben_Sensetal.txt")
vsa_lookup_pfad <- "inst/extdata/Tab_Substanzen.txt"
bafu_code_lookup_pfad <- "inst/extdata/BAFU_Liste_Parameter_Bezeichnungen_Datenaustausch.xlsx"

mvdaten_beispiel_mvwizr <- mvwizr::einlesen_mv_gbl(mv_daten_pfad, vsa_lookup_pfad, bafu_code_lookup_pfad)

usethis::use_data(mvdaten_beispiel_mvwizr, overwrite = TRUE)
