## code to prepare `rq_ue_beispiel_mvwizr` dataset goes here
rq_ue_beispiel_mvwizr <- berechne_rq_ue(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, kriterien_mvwizr)

usethis::use_data(rq_ue_beispiel_mvwizr, overwrite = TRUE)
