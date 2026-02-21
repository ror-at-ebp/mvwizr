# Interne Hilfsfunktionen für Einlesen ####


#' Einheiten von MV-Daten normalisieren
#'
#' Generische Funktion, um die Einheiten von MV-Daten zu normalisieren, d.h. für jede Messung dieselbe Einheit einzustellen.
#'
#' @param mvdata MV-Daten entsprechend der Spezifikationen.
#' @param wert Name der Spalte, die den Messwert enthält.
#' @param einheit Name der Spalte, die die Einheit enthält.
#' @param zieleinheit Zieleinheit: Eine von g/l, mg/l, µ/l, ng/l, pg/l (Vorgabe: µg/l). Achtung: Einheit in Qualitätskriterien-Tabelle beachten.
#'
#' @return Dataframe mit den normalisierten MV-Daten
#' @noRd
normalise_units <- function(mvdata, wert, einheit, zieleinheit = "\u00b5g/l") {
  einheit <- rlang::sym(einheit)

  # Umrechnungsfaktoren relativ zu g/l (Basiseinheit)
  einheiten_zu_gl <- c(
    "g/l" = 1,
    "mg/l" = 1e-3,
    "\u00b5g/l" = 1e-6,
    "ug/l" = 1e-6,
    "ng/l" = 1e-9,
    "pg/l" = 1e-12
  )

  stopifnot(zieleinheit %in% names(einheiten_zu_gl))
  ziel_zu_gl <- einheiten_zu_gl[zieleinheit]

  mvdata |> dplyr::mutate(
    # Bei dynamischen Variablennamen in mutate() muss := zur Zuweisung verwendet werden.
    {{ einheit }} := tolower({{ einheit }}),
    dplyr::across(
      .cols = dplyr::all_of(wert),
      .fns = ~ {
        quell_zu_gl <- einheiten_zu_gl[as.character({{ einheit }})]
        dplyr::if_else(!is.na(quell_zu_gl), .x * quell_zu_gl / ziel_zu_gl, .x)
      }
    ),
    {{ einheit }} := dplyr::if_else({{ einheit }} %in% names(einheiten_zu_gl), zieleinheit, {{ einheit }})
  )
}

guess_nawa_delim <- function(lines, filename, delims = c(",", "\t", ";")) {
  if (length(lines) == 0) {
    return("")
  }

  # blank text within quotes
  lines <- gsub('"[^"]*"', "", lines)

  splits <- lapply(delims, strsplit, x = lines, useBytes = TRUE, fixed = TRUE)

  counts <- lapply(splits, function(x) table(lengths(x)))

  num_fields <- vapply(counts, function(x) as.integer(names(x)[[1]]), integer(1))

  num_lines <- vapply(counts, function(x) (x)[[1]], integer(1))

  top_lines <- 0
  top_idx <- 0
  for (i in seq_along(delims)) {
    if (num_fields[[i]] >= 2 && num_lines[[i]] > top_lines ||
      (top_lines == num_lines[[i]] && (top_idx <= 0 || num_fields[[top_idx]] < num_fields[[i]]))) {
      top_lines <- num_lines[[i]]
      top_idx <- i
    }
  }
  if (top_idx == 0) {
    cli::cli_abort("Konnte keinen Delimiter f\u00fcr die Datei {nawa_mv} erraten. Bitte \u00fcberpr\u00fcfen Sie die Datei.")
  }

  delims[[top_idx]]
}

get_nawa_spec <- function(type = c("names", "colspecs"),
                          lang = c("DE", "FR")) {
  type <- match.arg(type)
  lang <- match.arg(lang)

  if (lang == "DE") {
    if (type == "names") {
      spec <- c(
        "Messstelle ID",
        "Messstelle Name",
        "Probenahme Ort",
        "Probenahme Art",
        "NAWA Probenahme Beginn (Datum und Uhrzeit)",
        "NAWA Probenahme Ende (Datum und Uhrzeit)",
        "Parameter",
        "Messwert",
        "Bestimmungsgrenze",
        "Einheit"
      )
    } else if (type == "colspecs") {
      spec <- structure(list(cols = list(`Messstelle ID` = structure(list(), class = c(
        "collector_double",
        "collector"
      )), `Messstelle Name` = structure(list(), class = c(
        "collector_character",
        "collector"
      )), `Probenahme Ort` = structure(list(), class = c(
        "collector_character",
        "collector"
      )), `Probenahme Art` = structure(list(), class = c(
        "collector_character",
        "collector"
      )), `NAWA Probenahme Beginn (Datum und Uhrzeit)` = structure(list(), class = c("collector_character", "collector")), `NAWA Probenahme Ende (Datum und Uhrzeit)` = structure(list(), class = c("collector_character", "collector")), Parameter = structure(list(), class = c(
        "collector_character",
        "collector"
      )), Messwert = structure(list(), class = c(
        "collector_character",
        "collector"
      )), Bestimmungsgrenze = structure(list(), class = c(
        "collector_double",
        "collector"
      )), Einheit = structure(list(), class = c(
        "collector_character",
        "collector"
      ))), default = structure(list(), class = c(
        "collector_character",
        "collector"
      )), delim = NULL), class = "col_spec")
    }
  } else if (lang == "FR") {
    if (type == "names") {
      spec <- c(
        "ID Station de mesure",
        "Nom Station de mesure",
        "Point d\'\u00e9chantillonnage",
        "Type d\'\u00e9chantillon",
        "NAWA D\u00e9but Echantillonnage (date et heure)",
        "NAWA Fin Echantillonnage (date et heure)",
        "Param\u00e8tre ID",
        "Valeur",
        "Limite de quantification",
        "Unit\u00e9"
      )
    } else if (type == "colspecs") {
      spec <- structure(
        list(
          cols = list(
            "ID Station de mesure" = structure(list(), class = c("collector_double", "collector")),
            "Nom Station de mesure" = structure(list(), class = c(
              "collector_character", "collector"
            )),
            "Point d\'\u00e9chantillonnage" = structure(list(), class = c(
              "collector_character", "collector"
            )),
            "Type d\'\u00e9chantillon" = structure(list(), class = c(
              "collector_character", "collector"
            )),
            "NAWA D\u00e9but Echantillonnage (date et heure)" = structure(
              list(format = "%d.%m.%Y %H:%M:%S"),
              class = c("collector_datetime", "collector")
            ),
            "NAWA Fin Echantillonnage (date et heure)" = structure(
              list(format = "%d.%m.%Y %H:%M:%S"),
              class = c("collector_datetime", "collector")
            ),
            "Param\u00e8tre ID" = structure(list(), class = c(
              "collector_character", "collector"
            )),
            Valeur = structure(list(), class = c(
              "collector_character", "collector"
            )),
            "Limite de quantification" = structure(list(), class = c("collector_double", "collector")),
            "Unit\u00e9" = structure(list(), class = c(
              "collector_character", "collector"
            ))
          ),
          default = structure(list(), class = c(
            "collector_character", "collector"
          )),
          delim = NULL
        ),
        class = "col_spec"
      )
    }
  }
  spec
}

check_nawa_excel_types <- function(mv_data) {
  if (!inherits(mv_data$BEGINNPROBENAHME, "POSIXct") || !inherits(mv_data$ENDEPROBENAHME, "POSIXct")) {
    cli::cli_alert_warning("Felder BEGINNPROBENAHME oder ENDEPROBENAHME wurden nicht als Datum eingelesen - versuche Typenkonvertierung.")
    mv_data <- mv_data |>
      dplyr::mutate(
        BEGINNPROBENAHME = as.POSIXct(.data$BEGINNPROBENAHME, format = "%d.%m.%Y %H:%M", tz = "Europe/Zurich"),
        ENDEPROBENAHME = as.POSIXct(.data$ENDEPROBENAHME, format = "%d.%m.%Y %H:%M", tz = "Europe/Zurich")
      )
  }
  if (!is.numeric(mv_data$Messwert) || !is.numeric(mv_data$Bestimmungsgrenze)) {
    cli::cli_alert_warning("Bestimmungsgrenzen wurden nicht als Zahlen eingelesen - versuche Typenkonvertierung.")
    mv_data <- mv_data |>
      dplyr::mutate(
        Bestimmungsgrenze = as.numeric(.data$Bestimmungsgrenze)
      )
  }

  mv_data
}

get_nawa_head_pos <- function(head_lines) {
  head_pos_de <- which(stringr::str_detect(head_lines, "Messstelle ID"))
  head_pos_fr <- which(stringr::str_detect(head_lines, "ID Station"))

  if (length(head_pos_de) == 0 && length(head_pos_fr) == 0) {
    cli::cli_abort(
      "Konnte keine Kopfzeile f\u00fcr DE oder FR in den ersten 30 Zeilen der Datei finden. Bitte \u00fcberpr\u00fcfen Sie die Datei."
    )
  }

  head_pos_de <- ifelse(rlang::is_empty(head_pos_de), 0, head_pos_de)
  head_pos_fr <- ifelse(rlang::is_empty(head_pos_fr), 0, head_pos_fr)

  list(
    head_pos_de = head_pos_de,
    head_pos_fr = head_pos_fr
  )
}

get_nawa_param_field <- function(mv_data, lang) {
  param_loc <- grep(pattern = "^Param", x = names(mv_data))[1]
  param_conform <- all(grepl(pattern = "^[A-Za-z0-9._-]+$", x = mv_data[[param_loc]]))

  if (param_conform) {
    parameter <- "BAFU_Parameter_ID"
  } else {
    parameter <- if (lang == "DE") {
      "BAFU_Bez_DE"
    } else {
      "BAFU_Bez_FR"
    }
  }
  cli::cli_alert_success("Erkannter Parameter: {parameter}.")

  parameter
}

rename_nawa_fields <- function(mv_data, parameter, lang) {
  col_renames <- c(
    "CODE",
    "STANDORT",
    "NAME",
    "PROBEARTID",
    "BEGINNPROBENAHME",
    "ENDEPROBENAHME",
    "PARAMETER",
    "Messwert",
    "Bestimmungsgrenze",
    "EINHEIT"
  )

  col_name_dict <- get_nawa_spec("names", lang = lang)

  names(col_name_dict) <- col_renames

  mv_data <- mv_data |>
    dplyr::rename(dplyr::all_of(col_name_dict)) |>
    dplyr::mutate(dplyr::across(-dplyr::all_of(col_renames), as.character))

  mv_data
}

sanitise_nawa_input <- function(mv_data) {
  reserved_names <- c(
    "CODE",
    "STANDORT",
    "NAME",
    "PROBEARTID",
    "BEGINNPROBENAHME",
    "ENDEPROBENAHME",
    "BAFU_Bez_DE",
    "BAFU_Bez_FR",
    "OPERATOR",
    "ID_Substanz",
    "WERT_NUM",
    "EINHEIT",
    "BG_max",
    "BG_min",
    "UID",
    "Jahr",
    "Tage",
    "P_\\w+",
    "I_\\w+",
    "V_\\w+",
    "S_\\w+",
    "Robustheit QK",
    "Name",
    "CQK",
    "AQK",
    "Informationen Recht",
    "RQ_\\w+",
    "Beurteilung_\\w+",
    "Ue_\\w+"
  )

  names_data <- names(mv_data)
  patterns <- paste0("^", reserved_names, "$")

  conflict_flags <- vapply(
    names_data,
    function(nm) any(stringr::str_detect(nm, patterns)),
    logical(1)
  )

  names_renamed <- ifelse(conflict_flags, paste0(names_data, "_input"), names_data)
  names(mv_data) <- names_renamed

  mv_data
}


#' Entferne Duplikate aus MV-Daten
#'
#' Entfernt Duplikate aus MV-Daten, indem jeweils der Datensatz mit dem höchsten Messwert für die gleiche Probe (Station, Beginn- und Enddatum) und Substanz verwendet wird. Duplikate werden anhand der Spalten `CODE`, `PROBEARTID`, `BEGINNPROBENAHME`, `ENDEPROBENAHME` und `PARAMETER` identifiziert.
#'
#' @param mv_data Dataframe mit MV-Daten, die Duplikate enthalten können.
#' @param dateiname Optionaler Dateiname, der in der Warnung ausgegeben wird, falls Duplikate gefunden werden (da Warnungen gesammelt am Schluss einer Skript-Ausführung erscheinen). Falls `NULL`, wird "MV-Daten" verwendet.
#' @param var_bg Name der Spalte mit den (tatsächlichen) Bestimmungsgrenze. Falls angegeben, wird bei Duplikaten, bei denen nicht der Messwert entscheidet (beide 0 oder identisch), der Wert mit der tieferen Bestimmungsgrenze verwendet. Überlegung: Wenn eine Methode eine viel tiefere Bestimmungsgrenze hat (z.B. GC-MS vs. LC-MS für Fipronil), dann wird ein Wert eher mit GC-MS gefunden. Um dann konsistent zu sein, wird die tiefere Bestimmungsgrenze bei Duplikaten mit Wert 0 (< BG) genommen.
#'
#' @returns Dataframe mit MV-Daten ohne Duplikate.
#' @noRd
entferne_duplikate <- function(mv_data, var_bg = NULL, dateiname = NULL) {
  if (!is.null(var_bg)) {
    if (!(var_bg %in% names(mv_data) && is.numeric(mv_data[[var_bg]]))) {
      cli::cli_warn("Variable f\u00fcr Bestimmungsgrenze {var_bg} nicht gefunden", class = "mvwizr_warn_missing_var_loq")
      var_bg <- "WERT_NUM"
    }
  } else {
    var_bg <- "WERT_NUM"
  }

  duplikate <- mv_data |>
    dplyr::group_by(
      .data$CODE,
      .data$PROBEARTID,
      .data$BEGINNPROBENAHME,
      .data$ENDEPROBENAHME,
      .data$PARAMETER
    ) |>
    dplyr::summarise(n = dplyr::n()) |>
    dplyr::ungroup()

  anz_duplikate <- nrow(duplikate |> dplyr::filter(.data$n > 1))

  if (!is.null(dateiname)) {
    dateiname <- basename(dateiname)
  } else {
    dateiname <- "MV-Daten"
  }

  if (anz_duplikate > 0) {
    cli::cli_warn(
      c("!" = "{dateiname}: Duplikate f\u00fcr {anz_duplikate} Datens\u00e4tze gefunden.", "i" = "Es wird jeweils der Datensatz (pro Station, Beginn-/Enddatum-zeit und Substanz) mit dem h\u00f6chsten Messwert verwendet."),
      class = "mvwizr_warn_dupes_found"
    )
  }

  # Bei Duplikaten Auswahl des höchsten Messwertes (dabei ist egal, ob dieser unter BG - der höchste Wert ist so oder so die konservativste Annahme)
  mv_data <- mv_data |>
    dplyr::group_by(
      .data$CODE, .data$PROBEARTID, .data$BEGINNPROBENAHME, .data$ENDEPROBENAHME, .data$PARAMETER
    ) |>
    dplyr::arrange(dplyr::desc(.data$WERT_NUM), .data[[var_bg]]) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  mv_data
}
