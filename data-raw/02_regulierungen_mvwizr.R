## Code um `regulierungen_mvwizr` zu erstellen

regulierungen_pfad <- "inst/extdata/anf_Substanz_recht.xlsx"

regulierungen_mvwizr <- mvwizr::einlesen_regulierungen(regulierungen_pfad)

usethis::use_data(regulierungen_mvwizr, overwrite = TRUE)
