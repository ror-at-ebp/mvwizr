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
#' Berechnet Plot-Limits, Date-Breaks und Jahresmarkierungen.
#'
#' Hinweis: Interne, nicht exportierte Funktion.
#'
#' @param mv_daten Daten mit BEGINNPROBENAHME-Spalte
#'
#' @return Benannte Liste mit limits, breaks, markierungen
#' @noRd
setup_date_axis <- function(mv_daten) {
  jahre <- lubridate::year(mv_daten$BEGINNPROBENAHME)
  plot_limits <- c(lubridate::as_datetime(paste0(min(jahre), "-01-01")), NA)
  date_breaks <- seq(
    plot_limits[1],
    lubridate::as_datetime(paste0(max(jahre), "-12-31")),
    by = "2 months"
  )
  markierungen_jahre <- lubridate::as_datetime(paste0(unique(jahre), "-12-31"))

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
  dplyr::if_else(is.na(dplyr::lag(x)) | !lubridate::year(dplyr::lag(x)) == lubridate::year(x),
    paste(lubridate::month(x, label = TRUE, locale = get_ch_locale()), "\n", lubridate::year(x)),
    paste(lubridate::month(x, label = TRUE, locale = get_ch_locale()))
  )
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
