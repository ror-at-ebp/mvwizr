# Stichproben plotten ####

#' Stationsübersicht Stichproben
#'
#' Diese Übersichtsfunktion plottet sämtliche Substanzen in allen Stichproben in einem als Raster über die Zeit und benutzt eine logarithmische Farbskala um relevante Substanzen und Veränderungen hervorzuheben.
#'
#' @inheritParams plot_misch_verlauf
#'
#' @return ggplot2 Plot-Objekt
#' @export
#'
#' @examples
#' # Stichprobenübersichten für zwei Stationen
#' plot_stich_uebersicht(mvdaten_beispiel_mvwizr, stationscode = "SA51")
#' plot_stich_uebersicht(mvdaten_beispiel_mvwizr, stationscode = "KI52")
plot_stich_uebersicht <- function(mv_daten,
                                  stationscode,
                                  jahr = NULL) {
  if (is.null(jahr)) {
    jahr <- unique(lubridate::year(mv_daten$BEGINNPROBENAHME))
  }

  stichproben <- mv_daten |>
    dplyr::filter(is.na(.data$ENDEPROBENAHME), !is.na(.data$ID_Substanz), .data$PROBEARTID == "S", .data$CODE %in% .env$stationscode, .data$WERT_NUM > 0, lubridate::year(.data$BEGINNPROBENAHME) %in% jahr) |>
    dplyr::mutate(BAFU_Bez_DE_fct = forcats::fct(.data$BAFU_Bez_DE, levels = stringr::str_sort(unique(.data$BAFU_Bez_DE), locale = get_lang())))

  if (nrow(stichproben) == 0) {
    cli::cli_abort("Keine Stichproben in Datensatz f\u00fcr Station {stationscode} gefunden.", class = "mvwizr_error_empty_dataset")
  }

  stationsname <- unique(stichproben$STANDORT)

  stichprobe_uebersicht_pobj <- ggplot2::ggplot(stichproben, ggplot2::aes(x = .data$BEGINNPROBENAHME, y = .data$BAFU_Bez_DE_fct, fill = .data$WERT_NUM)) +
    ggplot2::geom_tile() +
    # Log-Transformation für binned Skala
    ggplot2::scale_fill_viridis_b("Konz.\n \u00b5g/l", trans = scales::log_trans(base = 10)) +
    ggplot2::scale_x_datetime(
      "",
      breaks = "2 months",
      date_minor_breaks = "1 month",
      labels = lagged_labels_jahr,
    ) +
    # Reihenfolge der einträge auf y-Achse umkehren (z->a)
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::theme(
      legend.position = "bottom",
      # genügend Abstand zwischen Legendeneinträgen für Text
      legend.key.width = ggplot2::unit(2, "cm"),
      panel.background = ggplot2::element_rect(fill = "white"),
      panel.grid.major.x = ggplot2::element_line(colour = "grey80"),
      panel.grid.major.y = ggplot2::element_line(colour = "grey80"),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(colour = "black", fill = NA),
      strip.background = ggplot2::element_rect(fill = "white", colour = "black"),
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggtext::element_markdown(colour = "black")
    ) +
    ggplot2::ggtitle(sprintf("\u00dcbersicht \u00fcber Stichproben f\u00fcr Station %s", stationsname))

  stichprobe_uebersicht_pobj
}

#' Stichproben Konzentrationsverlauf
#'
#' @inheritParams plot_misch_verlauf
#' @param id_substanz Substanz(en), die geplottet werden sollen. Falls `NULL` werden sämtliche Substanzen geplottet.
#'
#' @return ggplot2 Plot-Objekt
#' @export
#'
#' @examples
#' # Plotten aller Substanzen für alle Stationen - sehr unübersichtlich
#' pobj <- plot_stich_verlauf(mvdaten_beispiel_mvwizr)
#'
#' # Allerdings eignet sich die Darstellung für die interaktive Analyse mit Plotly.
#' if (rlang::is_installed("plotly")) plotly::ggplotly(pobj)
plot_stich_verlauf <- function(mv_daten,
                               stationscode = NULL,
                               jahr = NULL,
                               id_substanz = NULL) {
  if (!is.null(stationscode)) {
    mv_daten <- dplyr::filter(mv_daten, .data$CODE %in% .env$stationscode)
  }

  if (!is.null(jahr)) {
    mv_daten <- dplyr::filter(mv_daten, lubridate::year(.data$BEGINNPROBENAHME) %in% .env$jahr)
  }

  if (!is.null(id_substanz)) {
    mv_daten <- dplyr::filter(mv_daten, .data$ID_Substanz %in% .env$id_substanz)
  }

  stichproben <- dplyr::filter(mv_daten, is.na(.data$ENDEPROBENAHME), !is.na(.data$ID_Substanz), .data$PROBEARTID == "S")

  # Substanzen auswählen, bei denen mindestens einmal etwas gemessen wurde
  substanzen_ueber0 <- stichproben |>
    dplyr::group_by(.data$STANDORT, .data$ID_Substanz) |>
    dplyr::summarise(ueber0 = sum(.data$WERT_NUM)) |>
    dplyr::filter(.data$ueber0 > 0) |>
    dplyr::mutate(ueber0 = TRUE)

  stichproben <- stichproben |>
    dplyr::left_join(substanzen_ueber0, by = c("STANDORT", "ID_Substanz")) |>
    dplyr::filter(.data$ueber0) |>
    dplyr::mutate(BAFU_Bez_DE_fct = forcats::fct(.data$BAFU_Bez_DE, levels = stringr::str_sort(unique(.data$BAFU_Bez_DE), locale = get_lang())))

  if (nrow(stichproben) == 0) {
    cli::cli_abort("Keine Stichproben in Datensatz f\u00fcr Station {stationscode} gefunden.", class = "mvwizr_error_empty_dataset")
  }

  ggplot2::ggplot(stichproben, ggplot2::aes(x = .data$BEGINNPROBENAHME, y = .data$WERT_NUM, colour = .data$BAFU_Bez_DE_fct)) +
    ggplot2::facet_grid(.data$STANDORT ~ ., scales = "free_y") +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_x_datetime(
      "",
      breaks = "2 months",
      date_minor_breaks = "1 month",
      labels = lagged_labels_jahr,
    ) +
    ggplot2::scale_y_continuous("Konz.\n \u00b5g/l") +
    plot_theme_proto() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    ) +
    ggplot2::ggtitle("Stichproben: Konzentrationsverlauf")
}

#' Zusammenfassung von Mischungstoxizitäten für Stichproben plotten
#'
#' Hinweis: Hierbei handelt es sich nur um einen Wrapper um `plot_misch_mixtox_verlauf` für dieselben Plots aber mit Stichproben.
#'
#' @inheritParams plot_misch_mixtox_verlauf
#'
#' @return ggplot2 Plot-Objekt
#' @export
#'
#' @examples
#' plot_stich_mixtox_zf(rq_ue_beispiel_mvwizr)
plot_stich_mixtox_zf <- function(rq_ue_daten,
                                 stationscode = NULL,
                                 jahr = NULL) {
  plot_misch_mixtox_verlauf(rq_ue_daten,
    stationscode = stationscode,
    jahr = jahr,
    modus = "kurzzeitig",
    plot_zusammenfassung = "stichproben"
  )
}
