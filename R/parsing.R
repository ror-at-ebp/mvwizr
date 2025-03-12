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
#' vsa_lookup_pfad <- system.file("extdata", "Tab_Substanzen.xlsx", package = "mvwizr")
#' bafu_filename <- "BAFU_Liste_Parameter_Bezeichnungen_Datenaustausch.xlsx"
#' bafu_code_pfad <- system.file("extdata", bafu_filename, package = "mvwizr")
#'
#' mvdaten <- einlesen_mv_gbl(mv_daten_pfad, vsa_lookup_pfad, bafu_code_pfad)
#'
einlesen_mv_gbl <- function(mv_daten_pfad, vsa_lookup_pfad, bafu_lookup_pfad, bSaP = FALSE) {
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

  mv_daten_df <- mv_daten_list %>%
    purrr::list_rbind() %>%
    # Explizite Schreibweise mit all_of() als Bedingung - alles andere mit everything() auswählen
    dplyr::select(dplyr::all_of(c(
      "CODE", "STANDORT", "NAME", "PROBEARTID", "BEGINNPROBENAHME",
      "ENDEPROBENAHME", "PARAMETERID_BAFU",
      "OPERATOR", "WERT_NUM", "EINHEIT", "MSTLTYP", "PARAMETERGRUPPEID",
      "PARAMETERGRUPPE"
    )), tidyr::everything()) %>%
    # Eindeutige ID für weitere Bearbeitung
    dplyr::mutate(UID = dplyr::row_number())

  # Alle weiteren Funktionen gehen davon aus, dass 1. die Einheiten normalisiert/alle gleich sind und 2. dass es sich um µg/l handelt.
  mv_daten_df <- einheiten_normalisieren(mv_daten_df, wert = "WERT_NUM", einheit = "EINHEIT", zieleinheit = "\u00b5g/l")

  # Duplikate: In den Testdaten gibt es für dieselben Samples für die gleichen Substanzen teilweise unterschiedliche Werte!
  duplikate <- mv_daten_df %>%
    dplyr::group_by(.data$CODE, .data$PROBEARTID, .data$BEGINNPROBENAHME, .data$ENDEPROBENAHME, .data$PARAMETERID_BAFU) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::ungroup()

  anz_duplikate <- nrow(duplikate %>% dplyr::filter(.data$n > 1))

  if (anz_duplikate > 0) {
    cli::cli_warn(c(
      "!" = "Duplikate f\u00fcr {anz_duplikate} Datens\u00e4tze gefunden.",
      "i" = "Es wird jeweils der Datensatz (pro Station, Beginn-/Enddatum-zeit und Substanz) mit dem h\u00f6chsten Messwert verwendet."
    ))
  }

  # Bei Duplikaten Auswahl des höchsten Messwertes (dabei ist egal, ob dieser unter BG - der höchste Wert ist so oder so die konservativste Annahme)
  mv_daten_df <- mv_daten_df %>%
    dplyr::group_by(.data$CODE, .data$PROBEARTID, .data$BEGINNPROBENAHME, .data$ENDEPROBENAHME, .data$PARAMETERID_BAFU) %>%
    # Ohne ties: Falls mehrmals der gleiche Wert, nehmen wir nur einen davon
    dplyr::slice_max(order_by = .data$WERT_NUM, n = 1, with_ties = FALSE) %>%
    dplyr::ungroup()

  alle_bezeichnungen <- unique(mv_daten_df$PARAMETERID_BAFU)
  nr_bezeichnungen <- length(alle_bezeichnungen)

  # Bestimmungsgrenzen werden aus allen eingelesenen Daten bestimmt => je länger Messreihe in Eingabedaten, desto grösser Unterschied zwischen min. und max. BG!
  bestimmungsgrenzen <- mv_daten_df %>%
    dplyr::filter(stringr::str_detect(.data$OPERATOR, "<")) %>%
    dplyr::select("PARAMETERID_BAFU", Bestimmungsgrenze = "WERT_NUM") %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$PARAMETERID_BAFU) %>%
    dplyr::summarise(BG_max = max(.data$Bestimmungsgrenze, na.rm = TRUE), BG_min = min(.data$Bestimmungsgrenze, na.rm = TRUE))

  nr_bestimmungsgrenzen <- nrow(bestimmungsgrenzen)

  # Aufgrund der Art, wie BG bei den MV-Daten des GBL angegeben sind (nur dort ersichtlich, wo Wert < BG), ist nicht für jede Substanz pro Messung die BG bekannt.
  if (!purrr::is_empty(setdiff(alle_bezeichnungen, bestimmungsgrenzen$PARAMETERID_BAFU))) {
    cli::cli_warn(
      c(
        "!" = "Nicht f\u00fcr alle Substanzen Bestimmungsgrenzen gefunden",
        "i" = "Es wurden maximale Bestimmungsgrenzen f\u00fcr {nr_bestimmungsgrenzen} von {nr_bezeichnungen} Substanzen gefunden."
      )
    )
  }

  # Join mit BAFU-ID (allerdings momentan nicht verwendet), VSA-ID und BG
  mv_daten_df <- dplyr::left_join(mv_daten_df, bafu_lookup_df, by = c("PARAMETERID_BAFU" = "BAFU_Parameter_ID"))
  mv_daten_df <- dplyr::left_join(mv_daten_df, vsa_lookup_df, by = c("PARAMETERID_BAFU" = "Parameter-ID"))
  mv_daten_df <- dplyr::left_join(mv_daten_df, bestimmungsgrenzen, by = "PARAMETERID_BAFU")

  # Identifizieren von Substanzen, bei denen die VSA ID fehlt
  fehlende_zuordnungen <- mv_daten_df %>%
    dplyr::filter(is.na(.data$ID_Substanz)) %>%
    dplyr::distinct(.data$PARAMETERID_BAFU) %>%
    unlist()
  nr_fehlende_zuo <- length(fehlende_zuordnungen)
  fehlende_zuordnungen <- paste(fehlende_zuordnungen, collapse = "; ")
  if (any(is.na(mv_daten_df$ID_Substanz))) {
    cli::cli_warn(
      c(
        "!" = "Nicht alle Substanzen, die in den MV-Daten gefunden wurden, konnten einer VSA ID_Substanz zugeordnet werden. Daten ohne ID_Substanz oder PARAMETERID_BAFU werden entfernt.",
        "i" = "{nr_fehlende_zuo} Fehlende Zuordnung{?en} f\u00fcr {fehlende_zuordnungen}"
      )
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
  ) %>%
    dplyr::arrange(.data$CODE, .data$BEGINNPROBENAHME, .data$PARAMETERID_BAFU) %>%
    dplyr::filter(!is.na(.data$ID_Substanz), !is.na(.data$PARAMETERID_BAFU)) %>%
    dplyr::mutate(UID = dplyr::row_number()) %>%
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
    return(entferne_berech_gbl(mv_daten_df))
  } else {
    return(mv_daten_df %>% dplyr::filter(.data$PROBEARTID != "bSaP"))
  }
}

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
#' @export
einheiten_normalisieren <- function(mvdata, wert, einheit, zieleinheit = "\u00b5g/l") {
  wert <- rlang::sym(wert)
  einheit <- rlang::sym(einheit)

  faktoren_ziele <- c(
    "g/l" = 1e-6,
    "mg/l" = 1e-3,
    "\u00b5g/l" = 1,
    "ng/l" = 1e3,
    "pg/l" = 1e6
  )

  stopifnot(faktoren_ziele[zieleinheit] > 0 && !is.na(faktoren_ziele[zieleinheit]))
  zielfaktor <- faktoren_ziele[zieleinheit]

  # Die Funktion ist so geschrieben, dass die Bezeichnung der Spalten angegeben werden kann/variabel ist.
  mvdata_normalisiert <- dplyr::mutate(
    mvdata,
    # Bei dynamischen Variablennamen in mutate() muss := zur Zuweisung verwendet werden.
    {{ wert }} := dplyr::case_when(
      {{ einheit }} == "pg/l" ~ {{ wert }} / 1e6,
      {{ einheit }} == "ng/l" ~ {{ wert }} / 1e3,
      {{ einheit }} == "mg/l" ~ {{ wert }} * 1e3,
      {{ einheit }} == "\u00b5g/l" ~ {{ wert }},
      {{ einheit }} == "ug/l" ~ {{ wert }}
    ),
    {{ wert }} := {{ wert }} * .env$zielfaktor,
    {{ einheit }} := .env$zieleinheit
  )
}

#' Regulierungsdaten einlesen
#'
#' @param regulierungen_pfad Pfad zur Exceldatei mit den Regulierungsdaten
#'
#' @return Dataframe mit Regulierungsinformationen
#' @export
einlesen_regulierungen <- function(regulierungen_pfad) {
  regulierungen_df <- readxl::read_excel(regulierungen_pfad) %>%
    dplyr::select(dplyr::all_of(c("ID_Substanz", "Name_reg" = "Name", "Informationen Recht", "GSCHV"))) %>%
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
  kriterien_df <- readxl::read_excel(kriterien_pfad) %>%
    # Umbenennen der CQK- und AQK-Spalten, damit kein Sonderzeichen (µ) im Namen enthalten ist => sonst schwierig für Verarbeitung
    dplyr::select(c(
      "ID_Substanz", "P_chron", "I_chron", "V_chron",
      "P_akut", "I_akut", "V_akut", "Robustheit QK", "Name"
    ), CQK = dplyr::starts_with("CQK"), AQK = dplyr::starts_with("AQK")) %>%
    dplyr::mutate(dplyr::across(c("ID_Substanz", "P_chron", "I_chron", "V_chron",
                                "P_akut", "I_akut", "V_akut", "Robustheit QK"), as.integer),
                  dplyr::across(c("CQK", "AQK"), as.numeric))

  if (any(is.na(kriterien_df$ID_Substanz))) {
    substanz_id_na <- kriterien_df |>
      dplyr::filter(is.na(.data$ID_Substanz)) |>
      dplyr::distinct() |>
      dplyr::pull(.data$Name)

    cli::cli_warn(
      c("!" = "Nicht alle Substanzen haben eine VSA ID_Substanz. Diese werden entfernt.",
        "i" = "Betroffene Substanzen: {substanz_id_na}")
      )
  }

  kriterien_df <- kriterien_df |>
    dplyr::filter(!is.na(.data$ID_Substanz))

  # Die VSA-IDs sind leider nicht eindeutig in der Qualitätskriterienliste, d.h. es gibt mehrere QK-Zeilen für einige VSA-IDs
  duplikate <- unlist(kriterien_df[duplicated(kriterien_df$ID_Substanz), "ID_Substanz"])
  anz_duplikate <- length(duplikate)
  dup_character <- paste(duplikate, collapse = ", ")

  if (!(purrr::is_empty(duplikate))) {
    cli::cli_warn(
      c(
        "!" = "Mehrfache Qualit\u00e4tskriterien f\u00fcr {anz_duplikate} Substanz_ID{?s} gefunden. Der tiefere Wert wird verwendet (CQK vor AQK).",
        "i" = "Betroffen: {dup_character}"
      )
    )
  }

  ohne_dupes <- kriterien_df[!(kriterien_df$ID_Substanz %in% duplikate), ]

  # Bei den Duplikaten den tieferen Wert bestimmen - wobei CQK Präzedenz hat vor AQK
  tiefere_werte <- kriterien_df[kriterien_df$ID_Substanz %in% duplikate, ] %>%
    dplyr::group_by(.data$ID_Substanz) %>%
    dplyr::mutate(
      rank_CQK = rank(.data$CQK, ties.method = "first", na.last = "keep"),
      rank_AQK = rank(.data$AQK, ties.method = "first", na.last = "keep"),
      use_value = dplyr::case_when(
        !is.na(.data$CQK) & .data$rank_CQK == 1L ~ TRUE,
        is.na(.data$CQK) & !is.na(.data$AQK) & .data$rank_AQK == 1L ~ TRUE,
        .default = FALSE
      )
    ) %>%
    dplyr::filter(.data$use_value) %>%
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
  vsa_lookup_df <- readxl::read_excel(vsa_lookup_pfad)

  if (!alle_felder) vsa_lookup_df <- dplyr::select(vsa_lookup_df, "ID_Substanz", "Parameter-ID") %>%
      dplyr::mutate(ID_Substanz = as.integer(.data$ID_Substanz))

  # Auch in der VSA Tab_Substanzen gibt es Duplikate bei der Parameter-ID (mehrere Parameter-ID pro VSA-ID).
  # Nicht ideal, aber die einzige triviale Lösung: Eine der beiden Substanz-IDs auswählen (Hier: die tiefere jeweils)
  duplikate_df <- vsa_lookup_df %>%
    dplyr::group_by(.data[["Parameter-ID"]]) %>%
    dplyr::filter(dplyr::n() > 1, !is.na(.data[["Parameter-ID"]]))
  duplikate_vec <- unlist(duplikate_df %>% dplyr::distinct(.data[["Parameter-ID"]]))
  anz_duplikate <- length(duplikate_vec)
  dup_character <- paste(sort(duplikate_vec), collapse = ", ")

  if (!(purrr::is_empty(duplikate_vec))) {
    cli::cli_warn(
      c(
        "!" = "{anz_duplikate} mehrfache vorhandene Bezeichnungen (VSA Parameter-ID) pro Substanz_ID gefunden. Die tiefere Substanz_ID wird verwendet.",
        "i" = "Betroffen: {dup_character}"
      )
    )
  }

  # Entfernen der Duplikate
  vsa_lookup_df <- vsa_lookup_df %>%
    dplyr::filter(!is.na(.data[["Parameter-ID"]])) %>%
    dplyr::arrange(.data$ID_Substanz) %>%
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
  bafu_lookup_df <- readxl::read_excel(bafu_lookup_pfad, col_types = "text") %>%
    dplyr::select(BAFU_Parameter_ID = dplyr::contains("Parameter-ID"), BAFU_Bez_DE = dplyr::contains("Deutsche Bezeichnung"), BAFU_Bez_FR = dplyr::contains("Franz\u00d6sische Bezeichnung"), !!switch_felder)
  bafu_lookup_df
}

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
berechne_rq_ue <- function(mv_daten, regulierungen, kriterien, robust3 = TRUE) {
  # Die Funktion geht davon aus, dass alle Substanzen eine VSA ID_Substanz haben
  joined_data <- mv_daten %>%
    dplyr::mutate(
      Jahr = lubridate::year(.data$BEGINNPROBENAHME),
      Tage = difftime(.data$ENDEPROBENAHME, .data$BEGINNPROBENAHME, units = "days")
    ) %>%
    dplyr::left_join(kriterien, by = "ID_Substanz") %>%
    dplyr::left_join(regulierungen, by = "ID_Substanz")

  # Für die Mischtoxizitäten können wir die RQ einfach mit einer 1 oder 0 multiplizieren, je nachdem, ob es einen Beitrag zur Mischtox gibt.
  rq_ue_data <- joined_data %>%
    dplyr::mutate(
      RQ_CQK = .data$WERT_NUM / .data$CQK,
      RQ_AQK = .data$WERT_NUM / .data$AQK,
      RQ_CQK_P = .data$RQ_CQK * .data$P_chron,
      RQ_CQK_I = .data$RQ_CQK * .data$I_chron,
      RQ_CQK_V = .data$RQ_CQK * .data$V_chron,
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
  Ue_robust_3 <- rq_ue_data %>%
    dplyr::filter(.data$`Robustheit QK` == 3, .data$Ue_AQK | .data$Ue_CQK) %>%
    dplyr::select(dplyr::all_of(c("CODE", "BEGINNPROBENAHME", "ENDEPROBENAHME", "BEZEICHNUNG_BAFU", "WERT_NUM", "Ue_AQK", "Ue_CQK")))

  if (nrow(Ue_robust_3) > 0 && robust3) {
    cli::cli_alert_info("Achtung: Folgende Proben \u00fcberschreiten QK mit Robustheit == 3 (nicht in Daten enthalten):")
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
  mixtox_data <- rq_data %>%
    dplyr::group_by(.data$CODE, .data$BEGINNPROBENAHME, .data$ENDEPROBENAHME, .data$Jahr, .data$Tage) %>%
    dplyr::summarise(
      Mix_Pflanzen_CQK = sum(.data$RQ_CQK_P, na.rm = TRUE),
      Mix_Invertebraten_CQK = sum(.data$RQ_CQK_I, na.rm = TRUE),
      Mix_Vertebraten_CQK = sum(.data$RQ_CQK_V, na.rm = TRUE),
      Mix_Pflanzen_AQK = sum(.data$RQ_AQK_P, na.rm = TRUE),
      Mix_Invertebraten_AQK = sum(.data$RQ_AQK_I, na.rm = TRUE),
      Mix_Vertebraten_AQK = sum(.data$RQ_AQK_V, na.rm = TRUE),
    ) %>%
    tidyr::pivot_longer(dplyr::starts_with("Mix"), names_prefix = "Mix_", names_sep = "_", names_to = c("Ziel", "Kriterium"), values_to = "RQ") %>%
    dplyr::mutate(
      Ziel = forcats::fct(.data$Ziel, levels = c("Vertebraten", "Invertebraten", "Pflanzen")),
      Ziel_num = as.integer(.data$Ziel),
      Beurteilung = dplyr::case_when(
        .data$RQ >= 0 & .data$RQ < 0.1 ~ "sehr gut",
        .data$RQ >= 0.1 & .data$RQ < 1 ~ "gut",
        .data$RQ >= 1 & .data$RQ < 2 ~ "m\u00e4ssig",
        .data$RQ >= 2 & .data$RQ < 10 ~ "unbefriedigend",
        .data$RQ > 10 ~ "schlecht",
        .data$RQ < 0 ~ "fehler!"
      ), Beurteilung = forcats::fct(.data$Beurteilung, levels = c("sehr gut", "gut", "m\u00e4ssig", "unbefriedigend", "schlecht"))
    ) %>%
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
    cli::cli_alert("Keine Stichproben im Datensatz gefunden.")
  }

  stichproben_agg <- stichproben |>
    dplyr::group_by(.data$CODE, .data$Jahr, .data$ID_Substanz, .data$PARAMETERGRUPPE, .data$PARAMETERGRUPPEID) |>
    dplyr::summarise(
      wert_perzentil = quantile(.data$WERT_NUM, probs = .env$perzentil, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$CODE, .data$Jahr, .data$PARAMETERGRUPPE, .data$PARAMETERGRUPPEID) |>
    dplyr::summarise(
      agg_summe = sum(.data$wert_perzentil, na.rm = TRUE)
    )

  stichproben_agg
}

# Interne Funktionen ####

#' Entferne SaP, welche für bSaP verwendet wurden
#'
#' Bei den (gemessenen) Mischproben unterscheidet das GBL zwischen 3.5-Tage Proben und 14-Tage Proben (SaP). Ausserdem können berechnete 14-Tage Proben (bSaP) aus 4 kürzeren Mischproben errechnet werden. Damit diese Werte nicht doppelt gezählt werden, sollten solche kürzeren Proben, die für die Berechnung verwendet wurden, aus den Daten entfernt werden.
#'
#' @param mv_daten Tibble mit den MV-Daten aus der Funktion `einlesen_mv_gbl`.
#'
#' Hinweis: Interne, nicht exportierte Funktion.
#'
#' @return mv-daten mit entfernten Datensätzen
#' @noRd
entferne_berech_gbl <- function(mv_daten) {
  # Zuerst bestimmen wir alle Intervalle, bei denen es berechnete Daten gibt
  berechnete_intervalle <- mv_daten %>%
    dplyr::filter(.data$PROBEARTID == "bSaP", !is.na(.data$ID_Substanz)) %>%
    dplyr::mutate(bSaP_Intervall = lubridate::interval(lubridate::as_date(.data$BEGINNPROBENAHME), lubridate::as_date(.data$ENDEPROBENAHME))) %>%
    dplyr::distinct(.data$CODE, .data$bSaP_Intervall, .data$ID_Substanz, .data$PARAMETERID_BAFU)

  # Dann überprüfen wir für jedes Sample (pro Substanz und Station), ob das Probenintervall im bSaP-Intervall liegt (Achtung: 3.5-Tage SaP-Proben, die ausserhalb des bSaP beginnen/enden und für die Berechnung verwendet wurden, werden so nicht gefunden!).
  SaP_zum_entfernen <- mv_daten %>%
    dplyr::left_join(berechnete_intervalle, by = c("CODE", "ID_Substanz", "PARAMETERID_BAFU")) %>%
    dplyr::mutate(In_bSaP = lubridate::interval(lubridate::as_date(.data$BEGINNPROBENAHME), lubridate::as_date(.data$ENDEPROBENAHME)) %within% .data$bSaP_Intervall) %>%
    dplyr::filter(.data$In_bSaP & .data$PROBEARTID == "SaP") %>%
    dplyr::pull("UID")

  mv_daten %>% dplyr::filter(!(.data$UID %in% .env$SaP_zum_entfernen))
}
