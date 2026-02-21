# Daten berechnen ####

#' Risikoquotienten (RQ) für Einzelstoffe berechnen
#'
#' @param mv_daten Normalisierte MV-Daten
#' @param regulierungen Regulierungsdaten (dataframe)
#' @param kriterien Qualitätskriterien (dataframe)
#' @param robust3 Logisch (Vorgabe: `TRUE`). Warnen, wenn QK mit Robustheit 3 überschritten sind (nicht in Daten enthalten)?
#'
#' @return Dataframe mit MV-Daten, ergänzt mit Risikoquotienten
#' @export
berechne_rq_ue <- function(mv_daten, regulierungen = NULL, kriterien = NULL, robust3 = TRUE) {
  # Falls regulierungen oder kriterien nicht angegeben werden, verwenden wir den Standardwert (= die mit dem Paket gebundelten Daten)
  regulierungen <- regulierungen %||% mvwizr::regulierungen_mvwizr
  kriterien <- kriterien %||% mvwizr::kriterien_mvwizr

  # Ergänzt, damit klare Fehlermeldung ausgegeben wird
  if (!is.data.frame(regulierungen)) {
    cli::cli_abort(c(
      "x" = "'regulierungen' muss ein data.frame oder tibble sein.",
      "i" = "Argument ist ein Objekt der Klasse {.cls {class(regulierungen)}}."
    ))
  }

  # Sicherstellen, dass kriterien ein data.frame ist
  if (!is.data.frame(kriterien)) {
    cli::cli_abort(c(
      "x" = "'kriterien' muss ein data.frame oder tibble sein.",
      "i" = "Argument ist ein Objekt der Klasse {.cls {class(kriterien)}}."
    ))
  }

  # Die Funktion geht davon aus, dass alle Substanzen eine VSA ID_Substanz haben
  joined_data <- mv_daten |>
    dplyr::mutate(
      Jahr = lubridate::year(.data$BEGINNPROBENAHME),
      Tage = difftime(.data$ENDEPROBENAHME, .data$BEGINNPROBENAHME, units = "days")
    ) |>
    dplyr::left_join(kriterien, by = "ID_Substanz") |>
    dplyr::left_join(regulierungen, by = "ID_Substanz")

  # Für die Mischtoxizitäten können wir die RQ einfach mit einer 1 oder 0 multiplizieren, je nachdem, ob es einen Beitrag zur Mischtox gibt.
  rq_ue_data <- joined_data |>
    dplyr::mutate(
      RQ_CQK = .data$WERT_NUM / .data$CQK,
      RQ_AQK = .data$WERT_NUM / .data$AQK,
      RQ_CQK_P = .data$RQ_CQK * .data$P_chron,
      RQ_CQK_I = .data$RQ_CQK * .data$I_chron,
      RQ_CQK_V = .data$RQ_CQK * .data$V_chron,
      RQ_CQK_S = .data$RQ_CQK * .data$S_chron,
      RQ_AQK_P = .data$RQ_AQK * .data$P_akut,
      RQ_AQK_I = .data$RQ_AQK * .data$I_akut,
      RQ_AQK_V = .data$RQ_AQK * .data$V_akut,
      Beurteilung_CQK = dplyr::case_when(
        .data$RQ_CQK >= 0 & .data$RQ_CQK < 0.1 ~ "sehr gut",
        .data$RQ_CQK >= 0.1 & .data$RQ_CQK < 1 ~ "gut",
        .data$RQ_CQK >= 1 & .data$RQ_CQK < 2 ~ "m\u00e4ssig",
        .data$RQ_CQK >= 2 & .data$RQ_CQK < 10 ~ "unbefriedigend",
        .data$RQ_CQK > 10 ~ "schlecht",
        .data$RQ_CQK < 0 ~ "fehler!"
      ),
      Beurteilung_AQK = dplyr::case_when(
        .data$RQ_AQK >= 0 & .data$RQ_AQK < 0.1 ~ "sehr gut",
        .data$RQ_AQK >= 0.1 & .data$RQ_AQK < 1 ~ "gut",
        .data$RQ_AQK >= 1 & .data$RQ_AQK < 2 ~ "m\u00e4ssig",
        .data$RQ_AQK >= 2 & .data$RQ_AQK < 10 ~ "unbefriedigend",
        .data$RQ_AQK > 10 ~ "schlecht",
        .data$RQ_AQK < 0 ~ "fehler!"
      ),
      # Terminologie: Ue_anhaltend und Ue_kurzzeitig betrifft Überschreitungen der Werte in der GSchV, während Ue_AQK und Ue_CQK alle Überschreitungen der QK umfassen
      dplyr::across(dplyr::starts_with("Beurteilung"), \(x) forcats::fct(x, levels = c("sehr gut", "gut", "m\u00e4ssig", "unbefriedigend", "schlecht"))),
      Ue_anhaltend = dplyr::if_else(.data$GSCHV == 1, .data$WERT_NUM > .data$CQK & .data$Tage >= 10, NA),
      Ue_kurzzeitig = dplyr::if_else(.data$GSCHV == 1, .data$WERT_NUM > .data$AQK, NA),
      Ue_spezifisch = dplyr::if_else(.data$GSCHV == 1, .data$Ue_anhaltend | .data$Ue_kurzzeitig, NA),
      Ue_generisch = dplyr::if_else(.data$GSCHV == 2, .data$WERT_NUM > 0.1, NA),
      Ue_AQK = .data$WERT_NUM > .data$AQK,
      Ue_CQK = .data$WERT_NUM > .data$CQK & .data$Tage >= 10
    )

  # Prüfen, ob es Überschreitungen bei Substanzen mit QK Robustheit 3 (unzuverlässiger Wert) gibt => als Warnung, dass diese Werte nicht in den Resultaten enthalten sind.
  Ue_robust_3 <- rq_ue_data |>
    dplyr::filter(.data$`Robustheit QK` == 3, .data$Ue_AQK | .data$Ue_CQK) |>
    dplyr::select(dplyr::all_of(c("CODE", "BEGINNPROBENAHME", "ENDEPROBENAHME", "PARAMETERID_BAFU", "WERT_NUM", "Ue_AQK", "Ue_CQK")))

  if (nrow(Ue_robust_3) > 0 && robust3) {
    cli::cli_warn("Achtung: Folgende Proben \u00fcberschreiten QK mit Robustheit == 3 (nicht in Daten enthalten):", class = "mvwizr_warn_ue_robust3")
    print(Ue_robust_3)
  }

  dplyr::filter(rq_ue_data, .data[["Robustheit QK"]] %in% c(1, 2))
}

#' Mischtoxizitäten berechnen
#'
#' @param rq_data Tibble mit Daten der Funktion berechne_rq_ue()
#'
#' @return Tibble mit Mischtoxizitäten
#' @export
berechne_mixtox <- function(rq_data) {
  mixtox_data <- rq_data |>
    dplyr::group_by(.data$CODE, .data$STANDORT, .data$BEGINNPROBENAHME, .data$ENDEPROBENAHME, .data$Jahr, .data$Tage) |>
    dplyr::summarise(
      Mix_Pflanzen_CQK = dplyr::if_else(any(!is.na(.data$RQ_CQK_P)), sum(.data$RQ_CQK_P, na.rm = TRUE), NA_real_),
      Mix_Invertebraten_CQK = dplyr::if_else(any(!is.na(.data$RQ_CQK_I)), sum(.data$RQ_CQK_I, na.rm = TRUE), NA_real_),
      Mix_Vertebraten_CQK = dplyr::if_else(any(!is.na(.data$RQ_CQK_V)), sum(.data$RQ_CQK_V, na.rm = TRUE), NA_real_),
      Mix_Bioakkumulation_CQK = dplyr::if_else(any(!is.na(.data$RQ_CQK_S)), sum(.data$RQ_CQK_S, na.rm = TRUE), NA_real_),
      Mix_Pflanzen_AQK = dplyr::if_else(any(!is.na(.data$RQ_AQK_P)), sum(.data$RQ_AQK_P, na.rm = TRUE), NA_real_),
      Mix_Invertebraten_AQK = dplyr::if_else(any(!is.na(.data$RQ_AQK_I)), sum(.data$RQ_AQK_I, na.rm = TRUE), NA_real_),
      Mix_Vertebraten_AQK = dplyr::if_else(any(!is.na(.data$RQ_AQK_V)), sum(.data$RQ_AQK_V, na.rm = TRUE), NA_real_)
    ) |>
    tidyr::pivot_longer(dplyr::starts_with("Mix"), names_prefix = "Mix_", names_sep = "_", names_to = c("Ziel", "Kriterium"), values_to = "RQ") |>
    dplyr::mutate(
      Ziel = forcats::fct(.data$Ziel, levels = c("Vertebraten", "Invertebraten", "Pflanzen", "Bioakkumulation")),
      Ziel_num = as.integer(.data$Ziel),
      Beurteilung = dplyr::case_when(
        .data$RQ >= 0 & .data$RQ < 0.1 ~ "sehr gut",
        .data$RQ >= 0.1 & .data$RQ < 1 ~ "gut",
        .data$RQ >= 1 & .data$RQ < 2 ~ "m\u00e4ssig",
        .data$RQ >= 2 & .data$RQ < 10 ~ "unbefriedigend",
        .data$RQ > 10 ~ "schlecht",
        .data$RQ < 0 ~ "fehler!",
        is.na(.data$RQ) ~ "nicht bewertet"
      ), Beurteilung = forcats::fct(.data$Beurteilung, levels = c("sehr gut", "gut", "m\u00e4ssig", "unbefriedigend", "schlecht", "nicht bewertet"))
    ) |>
    dplyr::ungroup()

  mixtox_data
}

#' Stichprobenauswertung GBL
#'
#' Wertet Stichproben nach Parametergruppe aus. Dazu wird pro Station und Jahr das Perzentil (Vorgabe: 90) der Werte pro Substanz berechnet. Danach wird die Summe der Perzentile pro Parametergruppe berechnet und zurückgegeben.
#'
#' @param mv_daten Tibble mit den MV-Daten aus der Funktion `einlesen_mv_gbl` resp gemäss Spezifikationen.
#' @param perzentil Perzentil, das berechnet werden soll pro Substanz (Vorgabe: 90).
#'
#' @return Tibble mit aggregierten Stichproben
#' @export
berechne_stichproben_gbl_aggregiert <- function(mv_daten, perzentil = 90) {
  perzentil <- perzentil / 100

  stichproben <- mv_daten |>
    dplyr::filter(is.na(.data$ENDEPROBENAHME)) |>
    dplyr::mutate(Jahr = lubridate::year(.data$BEGINNPROBENAHME))

  if (nrow(stichproben) == 0) {
    cli::cli_abort("Keine Stichproben im Datensatz gefunden.", class = "mvwizr_error_empty_dataset")
  }

  stichproben_agg <- stichproben |>
    dplyr::group_by(.data$CODE, .data$Jahr, .data$ID_Substanz, .data$PARAMETERGRUPPE, .data$PARAMETERGRUPPEID) |>
    dplyr::summarise(
      wert_perzentil = stats::quantile(.data$WERT_NUM, probs = .env$perzentil, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$CODE, .data$Jahr, .data$PARAMETERGRUPPE, .data$PARAMETERGRUPPEID) |>
    dplyr::summarise(
      agg_summe = sum(.data$wert_perzentil, na.rm = TRUE)
    )

  stichproben_agg
}
