# Interne Funktionen ####

#' Modus-Konfiguration für Plots
#' Gibt die korrekte Konfiguration (Symbole, Kriterium, Plot-Suffix) für den
#' gewählten Modus (andauernd/kurzzeitig) zurück.
#'
#' Hinweis: Interne, nicht exportierte Funktion.
#'
#' @param modus "andauernd" oder "kurzzeitig"
#'
#' @return Benannte Liste mit switchRQ, switchBeurteilung, switchKriterium, suffix_plot
#' @noRd
get_modus_config <- function(modus = c("andauernd", "kurzzeitig")) {
  modus <- match.arg(modus)
  switch(modus,
    "andauernd" = list(
      switchRQ = dplyr::sym("RQ_CQK"),
      switchBeurteilung = dplyr::sym("Beurteilung_CQK"),
      switchKriterium = "CQK",
      suffix_plot = " - chronisches Qualit\u00e4tskriterium"
    ),
    "kurzzeitig" = list(
      switchRQ = dplyr::sym("RQ_AQK"),
      switchBeurteilung = dplyr::sym("Beurteilung_AQK"),
      switchKriterium = "AQK",
      suffix_plot = " - akutes Qualit\u00e4tskriterium"
    )
  )
}

#' Datums-Achsen-Setup für Zeitreihen-Plots
#' Berechnet Plot-Limits, Date-Breaks und Jahresmarkierungen. Alle Zeitpunkte werden in der Zeitzone der
#' Daten erstellt, damit z.B. Proben mit Beginn am 01.01. um 00:00 (Europe/Zurich) nicht vor der Achsengrenze
#' liegen und abgeschnitten werden.
#'
#' Hinweis: Interne, nicht exportierte Funktion.
#'
#' @param mv_daten Daten mit BEGINNPROBENAHME-Spalte
#'
#' @return Benannte Liste mit limits, breaks, markierungen
#' @noRd
setup_date_axis <- function(mv_daten) {
  tz_daten <- lubridate::tz(mv_daten$BEGINNPROBENAHME)
  jahre <- lubridate::year(mv_daten$BEGINNPROBENAHME)
  plot_limits <- c(lubridate::as_datetime(paste0(min(jahre), "-01-01"), tz = tz_daten), NA)
  date_breaks <- seq(
    plot_limits[1],
    lubridate::as_datetime(paste0(max(jahre), "-12-31"), tz = tz_daten),
    by = "2 months"
  )
  markierungen_jahre <- lubridate::as_datetime(paste0(unique(jahre), "-12-31"), tz = tz_daten)

  list(
    limits = plot_limits,
    breaks = date_breaks,
    markierungen = markierungen_jahre
  )
}

#' Jahreslabels für ggplot2
#' Erstelle Labels für ggplot2 mit Jahr bei erstem Monat pro Jahr auf Achse.
#'
#' Hinweis: Interne, nicht exportierte Funktion.
#'
#' @param x Breaks von ggplot2 geom
#'
#' @return Labels für ggplot2
#' @noRd
lagged_labels_jahr <- function(x) {
  monate <- monat_labels(x)
  dplyr::if_else(is.na(dplyr::lag(x)) | !lubridate::year(dplyr::lag(x)) == lubridate::year(x),
    paste(monate, "\n", lubridate::year(x)),
    paste(monate)
  )
}

#' Abgekürzte Monatsnamen in Landessprache
#'
#' Verwendet die Schweizer Locale des Systems (`get_ch_locale()`). Ist diese
#' nicht installiert (z.B. Linux-Server, Docker, CI), wird ohne Warnung auf
#' fest hinterlegte Abkürzungen ausgewichen.
#'
#' Hinweis: Interne, nicht exportierte Funktion.
#'
#' @param x Datumsvektor
#' @param lang Sprache (`"de_CH"` oder `"fr_CH"`)
#'
#' @return Character-Vektor mit Monatsabkürzungen
#' @noRd
monat_labels <- function(x, lang = get_lang()) {
  locale <- get_ch_locale(lang)
  if (locale_verfuegbar(locale)) {
    as.character(lubridate::month(x, label = TRUE, locale = locale))
  } else {
    monate_fallback <- switch(lang,
      fr_CH = c("janv", "févr", "mars", "avr", "mai", "juin", "juil", "août", "sept", "oct", "nov", "déc"),
      c("Jan", "Feb", "Mär", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez")
    )
    monate_fallback[lubridate::month(x)]
  }
}

#' Prüft, ob eine Locale auf dem System gesetzt werden kann
#'
#' Hinweis: Interne, nicht exportierte Funktion.
#'
#' @param locale Locale-String
#'
#' @return `TRUE` falls die Locale gesetzt werden kann, sonst `FALSE`
#' @noRd
locale_verfuegbar <- function(locale) {
  if (is.null(locale)) {
    return(FALSE)
  }
  alt <- Sys.getlocale("LC_TIME")
  on.exit(Sys.setlocale("LC_TIME", alt), add = TRUE)
  ergebnis <- suppressWarnings(Sys.setlocale("LC_TIME", locale))
  !is.null(ergebnis) && nzchar(ergebnis)
}

#' Proto-ggplot2-theme
#' Basistheme für mvwizr-Plots
#'
#' Hinweis: Interne, nicht exportierte Funktion.
#'
#' @return ggplot2 theme-Layer
#' @noRd
plot_theme_proto <- function() {
  ggplot2::theme(
    legend.position = "bottom",
    panel.background = ggplot2::element_rect(fill = "white"),
    panel.grid.major.x = ggplot2::element_line(colour = "grey80"),
    panel.grid.major.y = ggplot2::element_line(colour = "grey80"),
    panel.grid.minor.x = ggplot2::element_blank(),
    panel.grid.minor.y = ggplot2::element_blank(),
    panel.border = ggplot2::element_rect(colour = "black", fill = NA),
    strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
  )
}

#' Ökotox-Farbskala
#' Farbskala für die Beurteilung gemäss Modulstufenkonzept.
#'
#' Hinweis: Interne, nicht exportierte Funktion.
#'
#' @return Benannter Vektor mit Farben
#' @noRd
farbskala_bewertung_ecotox <- function() {
  c("sehr gut" = "dodgerblue", "gut" = "chartreuse", "m\u00e4ssig" = "gold", "unbefriedigend" = "darkorange", "schlecht" = "firebrick1", "nicht bewertet" = "grey")
}

#' Bewertungsstufen f\u00fcr die Legende (ohne "nicht bewertet")
#' Gibt die f\u00fcnf Modulstufen-Bewertungen zur\u00fcck, die in der Legende erscheinen sollen. "nicht bewertet"
#' wird ausgeschlossen, da nicht bewertete Ziele als Datenl\u00fccke nicht dargestellt werden.
#'
#' Hinweis: Interne, nicht exportierte Funktion.
#'
#' @return Character-Vektor mit Bewertungsstufen
#' @noRd
farbskala_bewertung_ecotox_stufen <- function() {
  setdiff(names(farbskala_bewertung_ecotox()), "nicht bewertet")
}
