prep_scripts <- sort(dir("data-raw", pattern = "^\\d{2}_.*", full.names = TRUE))

lapply(prep_scripts, source)
