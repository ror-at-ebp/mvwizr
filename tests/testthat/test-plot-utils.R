datum <- as.Date(c("2023-11-01", "2023-12-01", "2024-01-01", "2024-03-01"))

test_that("lagged_labels_jahr liefert Labels mit Jahr beim Jahreswechsel", {
  labels <- lagged_labels_jahr(datum)
  expect_type(labels, "character")
  expect_length(labels, 4)
  expect_match(labels[c(1, 3)], "\n 202[34]$")
  expect_no_match(labels[c(2, 4)], "\n")
})

test_that("lagged_labels_jahr weicht bei fehlender Locale ohne Warnung aus", {
  local_mocked_bindings(get_ch_locale = function(...) "nicht_vorhanden")
  alte_locale <- Sys.getlocale("LC_TIME")
  withr::local_options(warn = 2)

  expect_false(locale_verfuegbar("nicht_vorhanden"))
  expect_no_warning(labels <- lagged_labels_jahr(datum))
  expect_equal(labels, c("Nov \n 2023", "Dez", "Jan \n 2024", "Mär"))
  expect_equal(Sys.getlocale("LC_TIME"), alte_locale)
})

test_that("monat_labels hat französischen Fallback", {
  local_mocked_bindings(locale_verfuegbar = function(...) FALSE)
  expect_equal(monat_labels(datum, lang = "fr_CH"), c("nov", "déc", "janv", "mars"))
})
