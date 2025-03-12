## Code um `kriterien_mvwizr` zu erstellen

kriterien_pfad <- "inst/extdata/Dat_Qual_kriterien.xlsx"

kriterien_mvwizr <- mvwizr::einlesen_kriterien(kriterien_pfad)

usethis::use_data(kriterien_mvwizr, overwrite = TRUE)
