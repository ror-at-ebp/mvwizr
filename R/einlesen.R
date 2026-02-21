# Daten einlesen ####

#' MV-Daten des GBL (Kt. Bern) einlesen
#'
#' Liest MV-Daten des GBL (Kt. Bern) ein und bereitet sie für die weitere Verarbeitung auf. Achtung: Falls `bSaP == TRUE`, werden 3.5-Tage-Mischproben, die im Intervall (Beginn- bis Enddatum ohne Zeit) der berechneten 14-Tage-Mischproben liegen, entfernt! Für die Auswertung akuter Überschreitungen muss deshalb `bSaP == FALSE` gesetzt werden.
#'
#' @param mv_daten_pfad Pfad zur kommaseparierten Datei mit den MV-Daten
#' @param vsa_lookup_pfad Pfad zur VSA-Tabelle `Tab_Substanzen.xlsx`
#' @param bafu_lookup_pfad Pfad zum BAFU-Namen-Lookup
#' @param bSaP Logisch (Vorgabe `FALSE`). Sollen berechnete Mischproben verwendet werden?
#'
#' @return Dataframe mit MV-Daten
#' @export
#'
#' @examples
#' mv_daten_pfad <- system.file("extdata", "Daten_MV_GBL_2019_2020.txt", package = "mvwizr")
#'
#' # Ab mvwizr v1.2 können vsa_lookup_pfad und bafu_lookup_pfad leer gelassen werden
#' # - es wird dann die mit dem Paket gebundelte Datei verwendet.
#' mvdaten <- einlesen_mv_gbl(mv_daten_pfad)
#'
einlesen_mv_gbl <- function(mv_daten_pfad, vsa_lookup_pfad = NULL, bafu_lookup_pfad = NULL, bSaP = FALSE) {
  # Falls die Argumente NULL sind, ersetzen wir sie durch den Standardwert - den Pfad zu den Dateien im Paket selber
  vsa_lookup_pfad <- vsa_lookup_pfad %||% system.file("extdata/Tab_Substanzen.txt", package = "mvwizr")
  bafu_lookup_pfad <- bafu_lookup_pfad %||% system.file("extdata/BAFU_Liste_Parameter_Bezeichnungen_Datenaustausch.xlsx", package = "mvwizr")

  vsa_lookup_df <- einlesen_vsa_lookup(vsa_lookup_pfad, alle_felder = FALSE)
  bafu_lookup_df <- einlesen_bafu_lookup(bafu_lookup_pfad)

  # Für die notwendigen Spalten Spezifikation angeben, damit früh ein Fehler signalisiert werden kann (z.B. bei fehlender Spalte oder falschem Format)
  spezifikation_mvdaten <- readr::cols(
    CODE = readr::col_character(),
    STANDORT = readr::col_character(),
    NAME = readr::col_character(),
    PROBEARTID = readr::col_character(),
    BEGINNPROBENAHME = readr::col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    ENDEPROBENAHME = readr::col_datetime(format = "%d.%m.%Y %H:%M:%S"),
    PARAMETERID_BAFU = readr::col_character(),
    BEZEICHNUNG_BAFU = readr::col_character(),
    OPERATOR = readr::col_character(),
    WERT_NUM = readr::col_double(),
    EINHEIT = readr::col_character(),
    MSTLTYP = readr::col_character(),
    PARAMETERGRUPPEID = readr::col_integer(),
    PARAMETERGRUPPE = readr::col_character(),
    .default = readr::col_character()
  )

  # Wichtig, damit die Zeitstempel richtig als CEST/CET eingelesen werden.
  deCH_locale <- readr::locale(encoding = "UTF-8", tz = "Europe/Zurich")

  # Loop über alle angegebenen MV-Daten zum Einlesen
  mv_daten_list <- purrr::map(mv_daten_pfad, function(mv_datei) {
    mv_df <- readr::read_delim(
      mv_datei,
      col_types = spezifikation_mvdaten,
      locale = deCH_locale,
      delim = ","
    )

    # Falls beim Parsen der Dateien Probleme auftreten, zeigen wir diese an und signalisieren anschliessend einen Fehler.
    mv_df_problems <- readr::problems(mv_df)
    if (nrow(mv_df_problems) > 0) {
      cli::cli_alert("Fehler: MV-Datei wurde nicht korrekt eingelesen: {mv_datei} ")
      print(mv_df_problems)
    }

    readr::stop_for_problems(mv_df)

    mv_df
  })

  mv_daten_df <- mv_daten_list |>
    purrr::list_rbind() |>
    # Explizite Schreibweise mit all_of() als Bedingung - alles andere mit everything() auswählen
    dplyr::select(
      dplyr::any_of(c("PARAMETER_GBL" = "PARAMETER")),
      dplyr::all_of(c(
        "CODE", "STANDORT", "NAME", "PROBEARTID", "BEGINNPROBENAHME",
        "ENDEPROBENAHME",
        "PARAMETER" = "PARAMETERID_BAFU",
        "OPERATOR", "WERT_NUM", "EINHEIT", "MSTLTYP", "PARAMETERGRUPPEID",
        "PARAMETERGRUPPE"
      )), tidyr::everything()
    )

  # Alle weiteren Funktionen gehen davon aus, dass 1. die Einheiten normalisiert/alle gleich sind und 2. dass es sich um µg/l handelt.
  mv_daten_df <- normalise_units(mv_daten_df, wert = "WERT_NUM", einheit = "EINHEIT", zieleinheit = "\u00b5g/l")

  # Duplikate: In den Testdaten gibt es für dieselben Samples für die gleichen Substanzen teilweise unterschiedliche Werte!
  mv_daten_df <- entferne_duplikate(mv_daten_df)

  alle_bezeichnungen <- unique(mv_daten_df$PARAMETER)
  nr_bezeichnungen <- length(alle_bezeichnungen)

  # Bestimmungsgrenzen werden aus allen eingelesenen Daten bestimmt => je länger Messreihe in Eingabedaten, desto grösser Unterschied zwischen min. und max. BG!
  bestimmungsgrenzen <- mv_daten_df |>
    dplyr::filter(stringr::str_detect(.data$OPERATOR, "<")) |>
    dplyr::select("PARAMETER", Bestimmungsgrenze = "WERT_NUM") |>
    dplyr::distinct() |>
    dplyr::group_by(.data$PARAMETER) |>
    dplyr::summarise(BG_max = max(.data$Bestimmungsgrenze, na.rm = TRUE), BG_min = min(.data$Bestimmungsgrenze, na.rm = TRUE))

  nr_bestimmungsgrenzen <- nrow(bestimmungsgrenzen)

  # Aufgrund der Art, wie BG bei den MV-Daten des GBL angegeben sind (nur dort ersichtlich, wo Wert < BG), ist nicht für jede Substanz pro Messung die BG bekannt.
  if (!purrr::is_empty(setdiff(alle_bezeichnungen, bestimmungsgrenzen$PARAMETER))) {
    cli::cli_warn(
      c(
        "!" = "Nicht f\u00fcr alle Substanzen Bestimmungsgrenzen gefunden",
        "i" = "Es wurden maximale Bestimmungsgrenzen f\u00fcr {nr_bestimmungsgrenzen} von {nr_bezeichnungen} Substanzen gefunden."
      ),
      class = "mvwizr_warn_missing_loq"
    )
  }

  # Join mit BAFU-ID (allerdings momentan nicht verwendet), VSA-ID und BG
  mv_daten_df <- dplyr::left_join(mv_daten_df, bafu_lookup_df, by = c("PARAMETER" = "BAFU_Parameter_ID"))
  mv_daten_df <- dplyr::left_join(mv_daten_df, vsa_lookup_df, by = c("PARAMETER" = "Parameter-ID"))
  mv_daten_df <- dplyr::left_join(mv_daten_df, bestimmungsgrenzen, by = "PARAMETER")

  # Identifizieren von Substanzen, bei denen die VSA ID fehlt
  fehlende_zuordnungen <- mv_daten_df |>
    dplyr::filter(is.na(.data$ID_Substanz)) |>
    dplyr::distinct(.data$PARAMETER) |>
    unlist()
  nr_fehlende_zuo <- length(fehlende_zuordnungen)
  fehlende_zuordnungen <- paste(fehlende_zuordnungen, collapse = "; ")
  if (any(is.na(mv_daten_df$ID_Substanz))) {
    cli::cli_warn(
      c(
        "!" = "Nicht alle Substanzen, die in den MV-Daten gefunden wurden, konnten einer VSA ID_Substanz zugeordnet werden. Daten ohne ID_Substanz oder PARAMETERID_BAFU werden entfernt.",
        "i" = "{nr_fehlende_zuo} Fehlende Zuordnung{?en} f\u00fcr {fehlende_zuordnungen}"
      ),
      class = "mvwizr_warn_missing_vsa_sid_match"
    )
  }

  # Werte unter BG auf 0 setzen; Substanzen mit fehlender BAFU Parameter-ID und VSA ID entfernen.
  mv_daten_df <- dplyr::mutate(
    mv_daten_df,
    OPERATOR = tidyr::replace_na(.data$OPERATOR, ""),
    Konz_inkl_BG = .data$WERT_NUM,
    WERT_NUM = dplyr::if_else(
      stringr::str_detect(.data$OPERATOR, "<"),
      0,
      .data$Konz_inkl_BG
    )
  ) |>
    dplyr::arrange(.data$CODE, .data$BEGINNPROBENAHME, .data$PARAMETER) |>
    dplyr::filter(!is.na(.data$ID_Substanz), !is.na(.data$PARAMETER)) |>
    dplyr::mutate(UID = dplyr::row_number()) |>
    dplyr::select(dplyr::all_of(c(
      "UID",
      "CODE",
      "STANDORT",
      "NAME",
      "PROBEARTID",
      "BEGINNPROBENAHME",
      "ENDEPROBENAHME",
      "ID_Substanz",
      "PARAMETERID_BAFU" = "PARAMETER",
      "OPERATOR",
      "WERT_NUM",
      "Konz_inkl_BG",
      "EINHEIT",
      "MSTLTYP",
      "PARAMETERGRUPPEID",
      "PARAMETERGRUPPE"
    )), tidyr::everything())

  # Falls berechnete Mischproben (bSaP) verwendet werden sollen, werden die kürzeren 3.5-Tage-Proben, die für die Berechnung verwendet wurden, entfernt, ansonsten die bSaP.
  if (bSaP) {
    return(prozessiere_bSaP(mv_daten_df, bSaP_identifier = "PROBEARTID"))
  } else {
    return(mv_daten_df |> dplyr::filter(.data$PROBEARTID != "bSaP"))
  }
}

#' Schreibe Import Manifest Vorlage für NAWA-MV-Daten
#'
#' @param import_manifest_file Pfad zur Import-Manifest-Datei, die geschrieben werden soll. Diese Datei wird im Excel-Format (.xlsx) geschrieben.
#' @param mv_datei_pfade Dateipfade zu den MV-Daten, die im NAWA-Format vorliegen. Falls angegeben, werden alle Dateien in das Manifest aufgenommen. Falls `NULL`, wird ein leeres Manifest geschrieben.
#'
#' @returns Gibt das Manifest-Template als tibble zurück.
#' @export
#'
#' @examples
#'
#' nawa_mv_pfade <- system.file("extdata", c(
#'   "NAWA_Bsp_1.xlsx",
#'   "NAWA_Bsp_2.xlsx", "NAWA_Bsp_3.csv", "NAWA_Bsp_4.xlsx",
#'   "NAWA_Bsp_5.csv"
#' ), package = "mvwizr")
#'
#' schreibe_nawa_import_manifest_template(tempfile(fileext = ".xlsx"),
#'   mv_datei_pfade = nawa_mv_pfade
#' )
#'
schreibe_nawa_import_manifest_template <- function(import_manifest_file, mv_datei_pfade = NULL) {
  cli::cli_alert_info("Schreibe Import-Manifest f\u00fcr NAWA-MV-Daten in {import_manifest_file}")

  if (is.null(mv_datei_pfade) || length(mv_datei_pfade) == 0) {
    cli::cli_abort(
      "Keine Dateien im NAWA-Format im Verzeichnis {mv_datei_pfade} gefunden oder Pfad inexistent."
    )
  } else if (any(!file.exists(mv_datei_pfade))) {
    cli::cli_abort(
      "Angegebene Datei(en) existieren nicht."
    )
  } else {
    cli::cli_alert_info(
      "Folgende Dateien werden dem Manifest hinzugef\u00fcgt:"
    )
    cli::cli_ul(mv_datei_pfade)
  }

  manifest_template <- tibble::tibble(
    file      = mv_datei_pfade,
    encoding  = NA_character_,
    header    = NA_integer_,
    delimiter = NA_character_,
    lang      = NA_character_
  )

  if (!dir.exists(dirname(import_manifest_file))) {
    cli::cli_abort("Verzeichnis {dirname(import_manifest_file)} existiert nicht.")
  }

  if (!grepl("\\.xlsx$", import_manifest_file, ignore.case = TRUE)) {
    cli::cli_abort("Die Zieldatei muss mit .xlsx enden: {import_manifest_file}")
  }

  if (file.exists(import_manifest_file)) {
    cli::cli_abort("Die Datei {import_manifest_file} existiert bereits.")
  }

  writexl::write_xlsx(manifest_template, import_manifest_file)

  invisible(manifest_template)
}

#' NAWA-MV-Daten einlesen
#'
#' Liest MV-Daten im BAFU-NAWA-Format ein. Falls die Datei einen Header aufweist vor dem Start der eigentlichen Tabelle, so wird dieser übersprungen. Um mehrere Dateien im NAWA-Format auf einmal einzulesen, kann die Funktion `batch_einlesen_nawa()` verwendet werden.
#'
#' Die Funktion liest entweder eine (einzelne) Datei im Excel (.xlsx)-Format ein oder als Text mit Trennzeichen (.csv, .txt). Die Funktion kann anstelle einer Datei auch ein Dataframe im NAWA-Format entgegennehmen (z.B. über Datenbank-Verbindung abgerufen).
#'
#' Duplikate (z.B. auch für Messungen auf mehreren Geräten) werden entfernt, indem jeweils der höchste Messwert für die gleiche Probe (Station, Beginn- und Enddatum) und Substanz verwendet wird.
#'
#' Bestimmungsgrenzen werden aus den eingelesenen Daten bestimmt, wobei die maximale und minimale Bestimmungsgrenze (über die gesamte Messperiode in den Daten) für jede Substanz ermittelt wird - die min. und max. Bestimmungsgrenzen sind also konstant im Datensatz. Falls eine Substanz keine Bestimmungsgrenze hat, so wird sie nicht entfernt, sondern es wird eine Warnung ausgegeben.
#'
#' Alle Einheiten werden (für die weiteren Berechnungen) automatisch auf µg/l normalisiert.
#'
#' Messungen von Substanzen, die nicht einer BAFU-Parameter-ID und einer VSA Substanz-ID zugeordnet werden können, werden entfernt.
#'
#' @param nawa_mv Entweder Pfad zur Datei im NAWA-Format (Excel oder Text) oder ein Dataframe im NAWA-Format (z.B. WISKI-Export)
#' @param vsa_lookup_pfad Pfad zur VSA-Tabelle `Tab_Substanzen.xlsx`
#' @param bafu_lookup_pfad Pfad zum BAFU-Namen-Lookup
#' @param delimiter Trennzeichen für Textdateien. Falls `NULL`, wird versucht, das Trennzeichen zu erraten. Standard ist `NULL`.
#' @param encoding Encoding der Textdatei. Falls `NULL`, wird versucht, das Encoding zu erraten. Standard ist `NULL`.
#' @param lang Sprache der Datei. Falls `NULL`, wird versucht, die Sprache zu erraten. Mögliche Werte sind "DE" (Deutsch) oder "FR" (Französisch). Standard ist `NULL`.
#' @param parameter Angabe, was im "Parameter"-Feld der MV-Datei steht (da nicht konsistent verwendet). Mögliche Werte sind "BAFU_Parameter_ID" (für den BAFU-Schlüssel ohne Sonderzeichen), "BAFU_Bez_DE" (Deutsche Bezeichnung) oder "BAFU_Bez_FR" (Französische Bezeichnung). Falls `NULL`, wird versucht, das Parameter-Feld zu erraten. Standard ist `NULL`.
#' @param header Zeilennummer, ab der die eigentliche Tabelle beginnt. Falls `NULL`, wird versucht, die Header-Zeile zu erraten. Standard ist `NULL`.
#'
#' @return Dataframe mit MV-Daten
#' @export
#'
#' @examples
#' # Testen der Einlesefunktion mit Heuristik
#' nawa_mv_fr <- system.file("extdata", "NAWA_Bsp_4.xlsx", package = "mvwizr")
#'
#' out <- einlesen_nawa(nawa_mv_fr)
#'
#' # Testen der Einlesefunktion mit Angabe von Argumenten
#' out2 <- einlesen_nawa(nawa_mv_fr,
#'   lang = "FR", parameter = "BAFU_Bez_FR",
#'   header = 8L
#' )
#'
#' # Vergleich der Resultate
#' identical(out, out2)
#'
einlesen_nawa <- function(nawa_mv,
                          vsa_lookup_pfad = NULL,
                          bafu_lookup_pfad = NULL,
                          delimiter = NA_character_,
                          encoding = NA_character_,
                          lang = NA_character_,
                          parameter = NA_character_,
                          header = NA_integer_) {
  # Falls die Argumente NULL sind, ersetzen wir sie durch den Standardwert - den Pfad zu den Dateien im Paket selber
  vsa_lookup_pfad <- vsa_lookup_pfad %||% system.file("extdata/Tab_Substanzen.txt", package = "mvwizr")
  bafu_lookup_pfad <- bafu_lookup_pfad %||% system.file("extdata/BAFU_Liste_Parameter_Bezeichnungen_Datenaustausch.xlsx", package = "mvwizr")

  vsa_lookup_df <- einlesen_vsa_lookup(vsa_lookup_pfad, alle_felder = FALSE)
  bafu_lookup_df <- einlesen_bafu_lookup(bafu_lookup_pfad)

  if (is.data.frame(nawa_mv)) {
    cli::cli_alert_info("Verarbeite MV-Daten als Dataframe.")
    type <- "dataframe"
  } else if (is.character(nawa_mv)) {
    if (length(nawa_mv) == 1 && file.exists(nawa_mv)) {
      file_ext <- tolower(tools::file_ext(nawa_mv))

      if (file_ext %in% c("xlsx", "xls")) {
        type <- "excel"
        cli::cli_alert_info("Lese MV-Daten von Excel-Datei {nawa_mv} ein.")
      } else if (file_ext %in% c("csv", "txt")) {
        type <- "text"
        cli::cli_alert_info("Lese MV-Daten von Text-Datei {nawa_mv} ein.")
      } else {
        cli::cli_abort(
          "Dateiformat von {nawa_mv} nicht unterst\u00fctzt. Bitte entweder Excel (xlsx/xls) oder CSV/Tab-Text-Datei (csv/txt) angeben."
        )
      }
    } else if (length(nawa_mv) > 1) {
      cli::cli_abort(
        "Mehrere Dateien angegeben oder Datei inexistent. Bitte entweder Dataframe oder eine einzelne Datei angeben."
      )
    }
  }

  if (type == "text") {
    if (is.na(encoding)) {
      cli::cli_alert_info("Versuche Encoding der Datei {nawa_mv} zu erraten.")
      encoding <- readr::guess_encoding(nawa_mv)[[1]][1]

      if (is.na(encoding)) {
        cli::cli_abort(
          "Encoding konnte nicht erraten werden. Bitte Encoding manuell angeben oder Datei \u00fcberpr\u00fcfen."
        )
      } else {
        cli::cli_alert_success("Erkanntes Encoding: {encoding}")
      }
    }

    if (is.na(delimiter) || is.na(header) || is.na(lang)) {
      head_lines <- readr::read_lines(nawa_mv,
        n_max = 30,
        locale = readr::locale(encoding = encoding, tz = "Europe/Zurich")
      )

      head_pos_list <- get_nawa_head_pos(head_lines)
      head_pos_de <- head_pos_list$head_pos_de
      head_pos_fr <- head_pos_list$head_pos_fr

      if (is.na(delimiter)) {
        cli::cli_alert_info("Versuche Trennzeichen der Datei {nawa_mv} zu erraten.")
        delimiter <- guess_nawa_delim(head_lines, filename = nawa_mv)
        cli::cli_alert_success("Erkanntes Trennzeichen: {delimiter}")
      }

      if (is.na(header)) {
        cli::cli_alert_info("Versuche Start des Tabellen-Headers der Datei {nawa_mv} zu erraten.")
        header <- head_pos_de + head_pos_fr
        cli::cli_alert_success("Erkannter Header-Start: Zeile {header}.")
      } else if (!(is.numeric(header))) {
        cli::cli_abort("Header muss eine Zahl sein.")
      }

      if (is.na(lang)) {
        cli::cli_alert_info("Versuche Sprache der Datei {nawa_mv} zu erraten.")
        lang <- if (head_pos_de > 0) {
          "DE"
        } else {
          "FR"
        }
        cli::cli_alert_success("Erkannte Sprache: {lang}.")
      } else if (!(lang %in% c("DE", "FR"))) {
        cli::cli_abort("Sprache muss entweder 'DE' oder 'FR' sein.")
      }
    }

    colspecs <- get_nawa_spec("colspecs", lang = lang)

    cli::cli_alert_info("Lese NAWA-MV-Daten von {nawa_mv} ein.")
    mv_data <- readr::read_delim(
      nawa_mv,
      delim = delimiter,
      locale = readr::locale(encoding = encoding, tz = "Europe/Zurich"),
      trim_ws = TRUE,
      skip = header - 1,
      col_types = colspecs,
      na = c("", "NA", "---"),
    )

    # Falls beim Parsen der Dateien Probleme auftreten, zeigen wir diese an und signalisieren anschliessend einen Fehler.
    mv_df_problems <- readr::problems(mv_data)
    if (nrow(mv_df_problems) > 0) {
      cli::cli_alert("Fehler: MV-Datei wurde nicht korrekt eingelesen: {nawa_mv} ")
      print(mv_df_problems)
    }

    readr::stop_for_problems(mv_data)

    mv_data <- sanitise_nawa_input(mv_data)

    if (is.na(parameter)) {
      cli::cli_alert_info("Versuche Parameter-Feld der Datei {nawa_mv} zu erraten.")
      parameter <- get_nawa_param_field(mv_data, lang = lang)
    } else if (!(parameter %in% c("BAFU_Parameter_ID", "BAFU_Bez_DE", "BAFU_Bez_FR"))) {
      cli::cli_abort("Parameter muss entweder 'BAFU_Parameter_ID', 'BAFU_Bez_DE' oder 'BAFU_Bez_FR' sein.")
    }

    # Wir lesen die Datumsfelder als Strings ein und wandeln danach um, weil die Datumsformate inkonsistent sein können (Z.T. ohne Zeitangabe)
    mv_data <- rename_nawa_fields(mv_data, parameter = parameter, lang = lang) |>
      dplyr::mutate(dplyr::across(
        c("BEGINNPROBENAHME", "ENDEPROBENAHME"),
        ~ lubridate::parse_date_time(.x, orders = c("dmY", "dmYHM", "dmYHMS"))
      ))
  }

  if (type == "excel") {
    if (is.na(header) || is.na(lang)) {
      suppressMessages({
        head_lines <- readxl::read_excel(
          nawa_mv,
          col_names = FALSE,
          col_types = "text",
          n_max = 30,
          .name_repair = function(x) {
            gsub("[\t\n\r]*", "", x)
          }
        )
      })

      head_lines <- unlist(head_lines)

      head_pos_list <- get_nawa_head_pos(head_lines)
      head_pos_de <- head_pos_list$head_pos_de
      head_pos_fr <- head_pos_list$head_pos_fr

      if (is.na(header)) {
        cli::cli_alert_info("Versuche Start des Tabellen-Headers der Datei {nawa_mv} zu erraten.")
        header <- head_pos_de + head_pos_fr
        cli::cli_alert_success("Erkannter Header-Start: Zeile {header}.")
      } else if (!(is.numeric(header))) {
        cli::cli_abort("Header muss eine Zahl sein.")
      }

      if (is.na(lang)) {
        cli::cli_alert_info("Versuche Sprache der Datei {nawa_mv} zu erraten.")
        lang <- if (head_pos_de > 0) {
          "DE"
        } else {
          "FR"
        }
        cli::cli_alert_success("Erkannte Sprache: {lang}.")
      } else if (!(lang %in% c("DE", "FR"))) {
        cli::cli_abort("Sprache muss entweder 'DE' oder 'FR' sein.")
      }
    }

    colspecs <- get_nawa_spec("names", lang = lang)

    mv_data <- readxl::read_excel(
      nawa_mv,
      skip = header - 1,
      guess_max = 1e7,
      .name_repair = function(x) {
        gsub("[\t\n\r]*", "", x)
      } # Bei Umbrüchen in Excel-Zellen...
    )

    mv_data <- sanitise_nawa_input(mv_data)

    if (is.na(parameter)) {
      cli::cli_alert_info("Versuche Parameter-Feld der Datei {nawa_mv} zu erraten.")
      parameter <- get_nawa_param_field(mv_data, lang = lang)
    } else if (!(parameter %in% c("BAFU_Parameter_ID", "BAFU_Bez_DE", "BAFU_Bez_FR"))) {
      cli::cli_abort("Parameter muss entweder 'BAFU_Parameter_ID', 'BAFU_Bez_DE' oder 'BAFU_Bez_FR' sein.")
    }

    mv_data <- rename_nawa_fields(mv_data, parameter = parameter, lang = lang)

    mv_data <- check_nawa_excel_types(mv_data)
  }

  if (type == "dataframe") {
    cli::cli_alert_info("Verarbeite MV-Daten als Dataframe (WISKI-Export).")
    lang <- "DE"
    parameter <- "pBAFU_ID"
    mv_data <- sanitise_nawa_input(mv_data)
    mv_data <- rename_nawa_fields(mv_data, parameter = parameter, lang = lang)
  }

  mv_data <- mv_data |>
    dplyr::mutate(
      PROBEARTID = dplyr::case_when(
        stringr::str_detect(PROBEARTID, "(?i)sammelprobe") ~ "SaP",
        stringr::str_detect(PROBEARTID, "(?i)composite") ~ "SaP",
        stringr::str_detect(PROBEARTID, "(?i)ponctuel") ~ "S",
        stringr::str_detect(PROBEARTID, "(?i)stichprobe") ~ "S",
        # Hier gibt es momentan nur diese Probenahme-Art; ggf. sollten hier bSaP und SaP unterschieden werden.
        TRUE ~ NA_character_
      ),
      WERT_NUM = dplyr::if_else(stringr::str_detect(.data$Messwert, "<"), "0", .data$Messwert),
      WERT_NUM = as.numeric(.data$WERT_NUM),
      # Type-casting nicht direkt in if_else(), da sonst eine Warnung erzeugt wird (da if_else die RHS immer evaluiert)
      WERT_NUM = dplyr::if_else((!is.na(.data$Bestimmungsgrenze) & .data$WERT_NUM < .data$Bestimmungsgrenze), 0, .data$WERT_NUM),
      CODE = as.character(.data$CODE),
      # Stationscode als Character
    )

  # Alle weiteren Funktionen gehen davon aus, dass 1. die Einheiten normalisiert/alle gleich sind und 2. dass es sich um µg/l handelt.
  cli::cli_alert_info("Normalisiere Einheiten der MV-Daten auf \u00b5g/l.")
  mv_data <- normalise_units(
    mv_data,
    wert = c("WERT_NUM", "Bestimmungsgrenze"),
    einheit = "EINHEIT",
    zieleinheit = "\u00b5g/l"
  )
  dateiname <- if (!is.data.frame(nawa_mv)) nawa_mv else NULL
  mv_data <- entferne_duplikate(mv_data, var_bg = "Bestimmungsgrenze", dateiname = dateiname)

  # Bestimmungsgrenzen werden aus allen eingelesenen Daten bestimmt => je länger Messreihe in Eingabedaten, desto grösser Unterschied zwischen min. und max. BG!
  cli::cli_alert_info("Max./min. Bestimmungsgrenzen der MV-Daten bestimmen...")
  bestimmungsgrenzen <- mv_data |>
    dplyr::select("PARAMETER", "Bestimmungsgrenze") |>
    dplyr::distinct() |>
    dplyr::group_by(.data$PARAMETER) |>
    dplyr::summarise(
      BG_max = max(.data$Bestimmungsgrenze, na.rm = TRUE),
      BG_min = min(.data$Bestimmungsgrenze, na.rm = TRUE)
    )

  # Join mit BAFU-ID (allerdings momentan nicht verwendet), VSA-ID und BG
  mv_data <- dplyr::left_join(mv_data, bestimmungsgrenzen, by = "PARAMETER")

  mv_data <- dplyr::left_join(mv_data,
    bafu_lookup_df,
    by = c("PARAMETER" = parameter)
  )

  switch(parameter,
    "BAFU_Parameter_ID" = {
      mv_data <- dplyr::rename(mv_data, BAFU_Parameter_ID = "PARAMETER")
    },
    "BAFU_Bez_DE" = {
      mv_data <- dplyr::rename(mv_data, BAFU_Bez_DE = "PARAMETER")
    },
    "BAFU_Bez_FR" = {
      mv_data <- dplyr::rename(mv_data, BAFU_Bez_FR = "PARAMETER")
    }
  )

  fehlende_zuordnungen_bafu <- mv_data |>
    dplyr::filter(is.na(.data$BAFU_Parameter_ID)) |>
    dplyr::distinct(.data[[parameter]]) |>
    dplyr::pull(.data[[parameter]])

  if (length(fehlende_zuordnungen_bafu) > 0) {
    cli::cli_warn(
      c(
        "!" = "{nawa_mv}: Nicht alle Substanzen, die in den MV-Daten gefunden wurden, konnten einer BAFU Parameter-ID zugeordnet werden.",
        "i" = "Es wurden {length(fehlende_zuordnungen_bafu)} Substanzen ohne BAFU Parameter-ID gefunden: {paste(fehlende_zuordnungen_bafu, collapse = '; ')}"
      ),
      class = "mvwizr_warn_missing_bafu_match"
    )
  }

  mv_data <- dplyr::left_join(mv_data,
    vsa_lookup_df,
    by = c("BAFU_Parameter_ID" = "Parameter-ID")
  ) |>
    dplyr::rename(PARAMETERID_BAFU = "BAFU_Parameter_ID")

  fehlende_zuordnungen_vsa <- mv_data |>
    dplyr::filter(!is.na(.data$PARAMETERID_BAFU), is.na(.data$ID_Substanz)) |>
    dplyr::distinct(.data$ID_Substanz, .data$PARAMETERID_BAFU) |>
    dplyr::pull(.data$PARAMETERID_BAFU)

  nr_fehlende_zuo <- length(fehlende_zuordnungen_vsa)
  fehlende_zuordnungen <- paste(fehlende_zuordnungen_vsa, collapse = "; ")

  if (nr_fehlende_zuo > 0) {
    cli::cli_warn(
      c(
        "!" = "{nawa_mv}: Nicht alle Substanzen, die in den MV-Daten gefunden wurden, konnten einer VSA ID_Substanz zugeordnet werden. Daten ohne ID_Substanz oder PARAMETERID_BAFU werden entfernt.",
        "i" = "{nr_fehlende_zuo} Fehlende Zuordnung{?en} f\u00fcr {fehlende_zuordnungen}"
      ),
      class = "mvwizr_warn_missing_vsa_sid_match"
    )
  }

  # Werte unter BG auf 0 setzen; Substanzen mit fehlender BAFU Parameter-ID und VSA ID entfernen.
  mv_data <- mv_data |>
    dplyr::arrange(.data$CODE, .data$BEGINNPROBENAHME, .data$PARAMETERID_BAFU) |>
    dplyr::filter(!is.na(.data$ID_Substanz), !is.na(.data$PARAMETERID_BAFU)) |>
    dplyr::mutate(UID = dplyr::row_number()) |>
    dplyr::select(dplyr::all_of(c(
      "UID",
      "CODE",
      "STANDORT",
      "NAME",
      "PROBEARTID",
      "BEGINNPROBENAHME",
      "ENDEPROBENAHME",
      "ID_Substanz",
      "PARAMETERID_BAFU",
      "BAFU_Bez_DE",
      "BAFU_Bez_FR",
      "WERT_NUM",
      "EINHEIT"
    )), tidyr::everything())
}

#' Batch-Einlesen von NAWA-MV-Daten
#'
#' Liest mehrere NAWA-MV-Daten-Dateien ein, entweder über ein Import-Manifest oder eine Liste von Dateipfaden. Die Funktion kann auch die Parameter wie Encoding, Header, Delimiter und Sprache erraten, wenn diese nicht angegeben sind (oder nur teilweise).
#'
#' @param nawa_mv_pfade Dateipfade zu MV-Daten im NAWA-Format (Excel oder Text). Falls `NULL`, muss das Import-Manifest verwendet werden.
#' @param vsa_lookup_pfad Pfad zur VSA-Tabelle `Tab_Substanzen.xlsx`
#' @param bafu_lookup_pfad Pfad zum BAFU-Namen-Lookup
#' @param import_manifest Pfad zu einem Import-Manifest, das die MV-Daten-Dateien und deren Parameter enthält. Falls `NULL`, müssen die Dateipfade bei `nawa_mv_pfade` angegeben werden und alle Parameter werden geraten. Das Manifest muss eine Excel-Datei sein, die die Spalten `file`, `encoding`, `header`, `delimiter` und `lang` enthält. Mittels `schreibe_nawa_import_manifest_template()` kann ein leeres Manifest erstellt werden, das dann mit den entsprechenden Dateien gefüllt werden kann.
#'
#' @returns Dataframe mit kombinierten MV-Daten aus allen angegebenen Dateien (Achtung: Entfernt keine Duplikate zwischen den Dateien, sondern nur innerhalb einer Datei).
#' @export
#'
#' @examples
#' # Batch-Einlesen mit Heuristik
#' # Die Zahlreichen Meldungen der Funktion sind hilfreich beim Auffinden von Problemen
#'
#' nawa_mv_pfade <- system.file("extdata", c(
#'   "NAWA_Bsp_1.xlsx",
#'   "NAWA_Bsp_2.xlsx", "NAWA_Bsp_3.csv", "NAWA_Bsp_4.xlsx",
#'   "NAWA_Bsp_5.csv"
#' ), package = "mvwizr")
#'
#' out <- batch_einlesen_nawa(nawa_mv_pfade)
#'
batch_einlesen_nawa <- function(nawa_mv_pfade = NULL,
                                vsa_lookup_pfad = NULL,
                                bafu_lookup_pfad = NULL,
                                import_manifest = NULL) {
  if (is.null(nawa_mv_pfade) && is.null(import_manifest)) {
    cli::cli_abort("Bitte entweder Pfade zu MV-Daten oder Import-Manifest angeben.")
  }

  if (!is.null(import_manifest)) {
    cli::cli_alert_info("Lese Import-Manifest {import_manifest} ein.")
    import_manifest_df <- readxl::read_excel(import_manifest) |>
      dplyr::select(dplyr::all_of(c("file", "encoding", "header", "delimiter", "lang"))) |>
      dplyr::mutate(idx = dplyr::row_number())

    if (nrow(import_manifest_df) == 0) {
      cli::cli_abort("Das Import-Manifest ist leer. Bitte f\u00fcgen Sie Dateien hinzu.")
    } else if (any(is.na(import_manifest_df$file))) {
      cli::cli_abort("Das Import-Manifest enth\u00e4lt leere Dateipfade. Bitte \u00fcberpr\u00fcfen Sie die Datei.")
    } else if (any(!file.exists(import_manifest_df$file))) {
      fehlende_dateien <- import_manifest_df$file[!file.exists(import_manifest_df$file)]
      cli::cli_abort(c(
        "Die folgenden Dateien im Import-Manifest existieren nicht:",
        cli::cli_ul(fehlende_dateien)
      ))
    }

    cli::cli_alert_info("Import-Manifest enth\u00e4lt {nrow(import_manifest_df)} Dateien. Lese diese ein...")

    mv_data_list <- purrr::pmap(
      import_manifest_df,
      function(file, encoding, header, delimiter, lang, idx) {
        cli::cli_h1("({idx}/{nrow(import_manifest_df)})  Lese {basename(file)} ...")
        einlesen_nawa(
          nawa_mv = file,
          vsa_lookup_pfad = vsa_lookup_pfad,
          bafu_lookup_pfad = bafu_lookup_pfad,
          encoding = encoding,
          header = header,
          delimiter = delimiter,
          lang = lang
        )
      },
      .progress = TRUE
    )
  } else {
    cli::cli_alert_info("Keine Import-Manifest-Datei angegeben. Lese Dateien ein und errate Funktionsparameter...")
    mv_data_list <- purrr::imap(
      nawa_mv_pfade,
      function(file, idx) {
        cli::cli_h1("({idx}/{length(nawa_mv_pfade)})  Lese {basename(file)} ...")
        einlesen_nawa(
          nawa_mv = file,
          vsa_lookup_pfad = vsa_lookup_pfad,
          bafu_lookup_pfad = bafu_lookup_pfad
        )
      },
      .progress = TRUE
    )
  }
  cli::cli_alert_info("Kombiniere MV-Daten aus {length(mv_data_list)} Dateien...")
  names(mv_data_list) <- basename(if (is.null(import_manifest)) {
    nawa_mv_pfade
  } else {
    import_manifest_df$file
  })

  mv_data_combined <- dplyr::bind_rows(mv_data_list, .id = "filename")
  cli::cli_alert_success("MV-Daten erfolgreich eingelesen und kombiniert.")
  mv_data_combined <- mv_data_combined |>
    dplyr::mutate(UID = dplyr::row_number())
}

#' Entferne SaP, welche für bSaP verwendet wurden
#'
#' Bei den (gemessenen) Mischproben unterscheidet das NAWA-MV-Protokoll zwischen 3.5-Tage-Proben und 14-Tage-Proben (SaP). Ausserdem können berechnete 14-Tage-Proben (bSaP) aus 4 kürzeren Mischproben errechnet werden. Damit diese Werte nicht doppelt gezählt werden, sollten solche kürzeren Proben, die für die Berechnung verwendet wurden, aus den Daten entfernt werden.
#'
#' Für GBL-Daten wird diese Funktion automatisch aufgerufen. Da beim NAWA-MV-Format zur Zeit keine Kennzeichnung von bSaP-Proben vorgesehen ist, muss dieser Schritt nach dem Einlesen erfolgen, und bSaP Proben müssen manuell gekennzeichnet werden.
#'
#' Achtung: Diese Berechnungen funktionieren nur, wenn die 3.5-Tage Proben zeitlich innerhalb der 14-Tage Proben liegen oder sich mit dem Zeitraum (auf Tag gerundet) decken. Falls die zur Berechnung verwendeten 3.5-Tage-Mischproben das Intervall der betreffenden bSaP 14-Tage-Probe überschneiden, so werden nur die 3.5-Tage-Proben, die innerhalb des Zeitraums liegen, entfernt!
#'
#' @param mv_daten Tibble mit den MV-Daten aus der Funktion `einlesen_mv_gbl` oder `einlesen_nawa` resp. `batch_einlesen_nawa`.
#'
#' @param bSaP_identifier Name der Spalte als String mit der Information, ob es sich um berechnete Mischproben ("bSaP") oder gemessene Mischproben ("SaP") handelt. Erlaubte Werte in Spalte: "bSaP", "SaP" und "S" (für Stichproben - werden nicht bearbeitet).
#'
#' @return mv-daten mit entfernten Datensätzen
#' @export
#'
#' @examples
#' # Bei NAWA-MV-Dateien muss der bSaP-Identifier manuell hinzugefügt werden.
#' # Im folgenden Beispiel werden zwei Dateien eingelesen:
#' # - NAWA_ohne_bSaP_Bsp.xlsx enthält gemessene Mischproben (3.5d und 14d)
#' # - NAWA_mit_bSaP_Bsp.csv enthält die berechneten Mischproben
#'
#' mv_bsap_pfade <- system.file("extdata", c(
#'   "NAWA_ohne_bSaP_Bsp.xlsx",
#'   "NAWA_mit_bSaP_Bsp.csv"
#' ), package = "mvwizr")
#' out <- batch_einlesen_nawa(mv_bsap_pfade) |>
#'   dplyr::mutate(PROBEARTID = dplyr::if_else(.data$filename ==
#'     "NAWA_mit_bSaP_Bsp.csv", "bSaP", .data$PROBEARTID))
#'
#' mv_bsap <- prozessiere_bSaP(out, bSaP_identifier = "PROBEARTID") |>
#'   dplyr::mutate(Dauer = difftime(.data$ENDEPROBENAHME,
#'     .data$BEGINNPROBENAHME,
#'     units = "days"
#'   ))
#'
prozessiere_bSaP <- function(mv_daten, bSaP_identifier) {
  identifiers_data <- unique(mv_daten[[bSaP_identifier]])
  identifiers_allowed <- c("SaP", "bSaP", "S")
  diff_identifiers <- setdiff(identifiers_data, identifiers_allowed)
  if (!(rlang::is_empty(diff_identifiers))) {
    cli::cli_abort(message = c(
      "Nur folgende Probenidentifier d\u00fcrfen verwendet werden: {identifiers_allowed}",
      "x" = "Nicht erlaubter Identifier in Spalte {bSaP_identifier} gefunden: {diff_identifiers}"
    ), class = "mvwizr_error_bSaP_identifier_ungueltig")
  }

  # Zuerst bestimmen wir alle Intervalle, bei denen es berechnete Daten gibt
  berechnete_intervalle <- mv_daten |>
    dplyr::filter(.data[[bSaP_identifier]] == "bSaP", !is.na(.data$ID_Substanz)) |>
    dplyr::mutate(bSaP_Intervall = lubridate::interval(lubridate::as_date(.data$BEGINNPROBENAHME), lubridate::as_date(.data$ENDEPROBENAHME))) |>
    dplyr::distinct(.data$CODE, .data$bSaP_Intervall, .data$ID_Substanz, .data$PARAMETERID_BAFU)

  # Dann überprüfen wir für jedes Sample (pro Substanz und Station), ob das Probenintervall im bSaP-Intervall liegt (Achtung: 3.5-Tage SaP-Proben, die ausserhalb des bSaP beginnen/enden und für die Berechnung verwendet wurden, werden so nicht gefunden!).
  SaP_zum_entfernen <- mv_daten |>
    dplyr::left_join(berechnete_intervalle, by = c("CODE", "ID_Substanz", "PARAMETERID_BAFU"), relationship = "many-to-many") |>
    dplyr::mutate(In_bSaP = lubridate::interval(lubridate::as_date(.data$BEGINNPROBENAHME), lubridate::as_date(.data$ENDEPROBENAHME)) %within% .data$bSaP_Intervall) |>
    dplyr::filter(.data$In_bSaP & .data[[bSaP_identifier]] == "SaP") |>
    dplyr::pull("UID")

  mv_daten |> dplyr::filter(!(.data$UID %in% .env$SaP_zum_entfernen))
}

#' Regulierungsdaten einlesen
#'
#' @param regulierungen_pfad Pfad zur Exceldatei mit den Regulierungsdaten
#'
#' @return Dataframe mit Regulierungsinformationen
#' @export
einlesen_regulierungen <- function(regulierungen_pfad) {
  regulierungen_df <- readxl::read_excel(regulierungen_pfad) |>
    dplyr::select(dplyr::all_of(c("ID_Substanz", "Name_reg" = "Name", "Informationen Recht", "GSCHV"))) |>
    dplyr::mutate(dplyr::across(c("ID_Substanz", "GSCHV"), as.integer))

  regulierungen_df
}

#' Qualitätskriterien einlesen
#'
#' Liest Qualitätskriterien ein. Bei mehreren Ökotoxikologischen Werten pro Substanz wird der tiefste Wert gewählt.
#'
#' @param kriterien_pfad Pfad zur Exceldatei mit den Qualitätskriterien
#'
#' @return Dataframe mit den Qualitätskriterien
#' @export
einlesen_kriterien <- function(kriterien_pfad) {
  kriterien_df <- readxl::read_excel(kriterien_pfad) |>
    # Umbenennen der CQK- und AQK-Spalten, damit kein Sonderzeichen (µ) im Namen enthalten ist => sonst schwierig für Verarbeitung
    dplyr::select(c(
      "ID_Substanz", "P_chron", "I_chron", "V_chron", "S_chron",
      "P_akut", "I_akut", "V_akut", "Robustheit QK", "Name"
    ), CQK = dplyr::starts_with("CQK"), AQK = dplyr::starts_with("AQK")) |>
    dplyr::mutate(
      dplyr::across(c(
        "ID_Substanz", "P_chron", "I_chron", "V_chron",
        "P_akut", "I_akut", "V_akut", "Robustheit QK"
      ), as.integer),
      dplyr::across(c("CQK", "AQK"), as.numeric)
    )

  if (any(is.na(kriterien_df$ID_Substanz))) {
    substanz_id_na <- kriterien_df |>
      dplyr::filter(is.na(.data$ID_Substanz)) |>
      dplyr::distinct() |>
      dplyr::pull(.data$Name)

    cli::cli_warn(
      c(
        "!" = "Nicht alle Substanzen haben eine VSA ID_Substanz. Diese werden entfernt.",
        "i" = "Betroffene Substanzen: {substanz_id_na}"
      ),
      class = "mvwizr_warn_dat_qual_missing_vsa_sid"
    )
  }

  kriterien_df <- kriterien_df |>
    dplyr::filter(!is.na(.data$ID_Substanz))

  # Die VSA-IDs sind möglicherweise nicht eindeutig in der Qualitätskriterienliste, d.h. es gibt mehrere QK-Zeilen für einige VSA-IDs
  duplikate <- unlist(kriterien_df[duplicated(kriterien_df$ID_Substanz), "ID_Substanz"])
  anz_duplikate <- length(duplikate)
  dup_character <- paste(duplikate, collapse = ", ")

  if (!(purrr::is_empty(duplikate))) {
    cli::cli_warn(
      c(
        "!" = "Mehrfache Qualit\u00e4tskriterien f\u00fcr {anz_duplikate} Substanz_ID{?s} gefunden. Der tiefere Wert wird verwendet (CQK vor AQK).",
        "i" = "Betroffen: {dup_character}"
      ),
      class = "mvwizr_warn_dat_qual_dupes"
    )
  }

  ohne_dupes <- kriterien_df[!(kriterien_df$ID_Substanz %in% duplikate), ]

  # Bei den Duplikaten den tieferen Wert bestimmen - wobei CQK Präzedenz hat vor AQK
  tiefere_werte <- kriterien_df[kriterien_df$ID_Substanz %in% duplikate, ] |>
    dplyr::group_by(.data$ID_Substanz) |>
    dplyr::mutate(
      rank_CQK = rank(.data$CQK, ties.method = "first", na.last = "keep"),
      rank_AQK = rank(.data$AQK, ties.method = "first", na.last = "keep"),
      use_value = dplyr::case_when(
        !is.na(.data$CQK) & .data$rank_CQK == 1L ~ TRUE,
        is.na(.data$CQK) & !is.na(.data$AQK) & .data$rank_AQK == 1L ~ TRUE,
        .default = FALSE
      )
    ) |>
    dplyr::filter(.data$use_value) |>
    dplyr::select(-c("rank_CQK", "rank_AQK", "use_value"))

  kriterien_df <- dplyr::bind_rows(ohne_dupes, tiefere_werte)

  kriterien_df
}

#' VSA ID_Substanz Lookup einlesen
#'
#' @param vsa_lookup_pfad Pfad zur Excel-Datei mit dem ID_Substanz-Lookup des VSA
#' @param alle_felder Alle Spalten exportieren? Standardmässig werden nur die im Weiteren benötigten Spalten exportiert.
#'
#' @return Dataframe mit Lookups
#' @export
einlesen_vsa_lookup <- function(vsa_lookup_pfad, alle_felder = FALSE) {
  vsa_colspec <- readr::cols(
    ID_Substanz = readr::col_integer(),
    Name = readr::col_character(),
    `Parameter-ID` = readr::col_character(),
    `CASNr - Gemisch` = readr::col_character(),
    Kommentar = readr::col_character(),
    Chem_ID_OZ = readr::col_character(),
    Micropol_ID = readr::col_character(),
    AnalytikDB_ID = readr::col_character(),
    PPDB_ID = readr::col_character(),
    `relevante Isomere` = readr::col_character(),
    InChIKey = readr::col_character()
  )

  vsa_lookup_df <- readr::read_delim(vsa_lookup_pfad, delim = ";", quote = '"', col_types = vsa_colspec, locale = readr::locale(encoding = "UTF-8"))

  if (!alle_felder) {
    vsa_lookup_df <- dplyr::select(vsa_lookup_df, "ID_Substanz", "Parameter-ID")
  }

  # Auch in der VSA Tab_Substanzen gibt es Duplikate bei der Parameter-ID (mehrere Parameter-ID pro VSA-ID).
  # Nicht ideal, aber die einzige triviale Lösung: Eine der beiden Substanz-IDs auswählen (Hier: die tiefere jeweils)
  duplikate_df <- vsa_lookup_df |>
    dplyr::group_by(.data[["Parameter-ID"]]) |>
    dplyr::filter(dplyr::n() > 1, !is.na(.data[["Parameter-ID"]]))
  duplikate_vec <- unlist(duplikate_df |> dplyr::distinct(.data[["Parameter-ID"]]))
  anz_duplikate <- length(duplikate_vec)
  dup_character <- paste(sort(duplikate_vec), collapse = ", ")

  if (!(purrr::is_empty(duplikate_vec))) {
    cli::cli_warn(
      c(
        "!" = "VSA-Lookup: {anz_duplikate} mehrfache Bezeichnungen (VSA Parameter-ID) pro Substanz_ID gefunden. Verwende tiefere Substanz_ID.",
        "i" = "Betroffen: {dup_character}"
      ),
      class = "mvwizr_warn_vsa_dup_sid"
    )
  }

  # Entfernen der Duplikate
  vsa_lookup_df <- vsa_lookup_df |>
    dplyr::filter(!is.na(.data[["Parameter-ID"]])) |>
    dplyr::arrange(.data$ID_Substanz) |>
    dplyr::distinct(.data[["Parameter-ID"]], .keep_all = TRUE)

  vsa_lookup_df
}

#' BAFU-Namens-Lookup einlesen
#'
#' @param bafu_lookup_pfad Pfad zur Exceldatei mit dem Namens-Lookup
#' @param alle_felder Logisch (Vorgabe: `FALSE`). Sollen alle Spalten der Tabelle exportiert werden?
#'
#' @return Dataframe mit Zuordnung der französischen und deutschen Bezeichnungen zur eindeutigen BAFU Parameter-ID
#' @export
einlesen_bafu_lookup <- function(bafu_lookup_pfad, alle_felder = FALSE) {
  if (alle_felder) {
    switch_felder <- dplyr::expr(tidyr::everything())
  } else {
    switch_felder <- NULL
  }

  # Spalten umbenannt, da Bezeichnungen viel zu lang und gespickt mit Sonderzeichen in Originaltabelle
  bafu_lookup_df <- readxl::read_excel(bafu_lookup_pfad, col_types = "text") |>
    dplyr::select(BAFU_Parameter_ID = dplyr::contains("Parameter-ID"), BAFU_Bez_DE = dplyr::contains("Deutsche Bezeichnung"), BAFU_Bez_FR = dplyr::contains("Franz\u00d6sische Bezeichnung"), !!switch_felder)
  bafu_lookup_df
}
