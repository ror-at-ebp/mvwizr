# Mischproben plotten ####

## Konzentrationen ####

#' Verlauf von Mischproben plotten
#'
#' Plottet den Konzentrationsverlauf von Mikroverunreinigungen für eine bestimmte Station und Zeitraum. Dabei werden Stichproben (Proben ohne Enddatum) entfernt.
#'
#' Es sind in Abhängigkeit des Modus (`id_substanz`) verschiedene Plottypen `plot_typ` erlaubt:
#'
#' \itemize{
#'   \item `"barplot"`: Monochromer Barplot; entweder für 1 Substanz oder Summenplots. Es werden auch überlappende Messintervalle dargestellt.
#'   \item `"kombiniert"`: Eine Kombination von `"barplot"` und `"treppen"` zur Diagnose/Kontrolle.
#'   \item `"striche"`: Horizontale Striche; entweder für 1 oder mehrere Substanzen. Es werden auch überlappende Messintervalle dargestellt.
#'   \item `"treppen"`: Treppenplot (ähnlich GeomStep). Kurze Messdauern <10 Tagen werden für diese Darstellung entfernt, um die Wahrscheinlichkeit von Überlappungen zu minimieren. Falls dennoch zeitliche Überlappungen existieren, stoppt die Funktion mit einem Fehler.
#'   \item `"barplot_gruppen"`: Nur für Summenplots (`id_substanz` = `NULL`), wenn `plot_parametergruppe` (siehe unten) angegeben ist und diese Spalte in den Daten existiert. Zeigt Barplots gruppiert nach einer bestimmten Kategorisierung (z.B. Aufteilung in Pestizide, Haushaltschemikalien und Arzneimittel). Achtung: Damit Stoffe unabhängig vom Zulassungsstatus gezeigt werden, muss bei `zulassungstyp` "Alle" gesetzt sein.
#' }
#'
#' @param mv_daten Dataframe mit aufbereiteten MV-Daten gemäss Spezifikation
#' @param regulierungen Dataframe mit aufbereiteten Regulierungs-Daten gemäss Spezifikation
#' @param stationscode Station, für welche der Plot erstellt werden soll
#' @param jahr Jahre (numerischer Vektor), für welche ein Verlauf geplottet werden soll. Falls `NULL` (Vorgabewert), werden alle verfügbaren Daten geplottet.
#' @param id_substanz Legt fest, ob für eine Substanz (Vektorlänge = 1; z.B. `id_substanz = 71`), mehrere Substanzen (Vektorlänge > 1; z.B. `id_substanz = c(71, 91)`) oder alle Substanzen (Summen) der Verlauf geplottet werden soll. Falls `NULL` (Vorgabewert) wird ein Summenplot erstellt
#' @param zulassungstyp Filtert nach der Zulassungsart (Spalte `Informationen Recht` in Regulierungstabelle). Wird als regulärer Ausdruck interpretiert. Der Vorgabewert filtert nach Bioziden und Pflanzenschutzmitteln (auch mit ausgelaufener Zulassung). Für die Darstellung aller Substanzen (z.B. für gruppierten Barplot) "Alle" verwenden. Falls nur bestimmte Zulassungsschlüssel verwendet werden sollen, dies als regulären Ausdruck angeben (z.B. `zulassungstyp = "B|PX"` für zugelassene Biozide und/oder nicht mehr zugelassene PSM).
#' @param plot_typ Plottyp in Abhängigkeit des gewählten Modus bei `id_substanz`.
#' \itemize{
#'   \item Falls `length(id_substanz) == 1`, sind die folgenden Werte erlaubt: `"barplot"`, `"striche"`, `"treppen"`. Wenn vorhanden, werden minimale und maximale Bestimmungsgrenzen geplottet
#'   \item Falls `length(id_substanz) > 1`, sind die folgenden Werte erlaubt: `"striche"`, `"treppen"`. Es werden keine Bestimmungsgrenzen geplottet.
#'   \item Falls keine `id_substanz` angegeben wird, werden Summenplots gezeichnet. Folgende Werte sind erlaubt: `"barplot"`, `"kombiniert"`, `"treppen"`. `"kombiniert"` stellt eine Kombination der anderen beiden Plottypen dar (z.B. für Kontrollzwecke).
#' }
#' @param plot_bg Logisch (Vorgabe: `TRUE`). Sollen Bestimmungsgrenzen, falls vorhanden, gezeichnet werden?
#' @param plot_parametergruppe Name der Spalte (String), die für das Kategorisieren gruppierter Barplots (Summenplots) verwendet werden soll. Für die Daten des GBL Bern "PARAMETERGRUPPE" verwenden.
#' @param bg_typ Falls `"minmax"` (Standard), werden minmale und maximale Bestimmungsgrenzen als gestrichelte Linie geplottet. Falls `"effektiv"` und falls die Spalte `"Bestimmungsgrenze` in den Daten vorhanden ist, so werden effektive Bestimmungsgrenzen als hellblaue teiltransparente Säulen geplottet.
#'
#' @return ggplot2 Plot-Objekt
#' @export
#'
#' @examples
#' # Summenplot mit Barplot-Darstellung
#' plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr, "URT010")
#'
#' # Summenplot mit Treppen-Darstellung
#' plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr,
#'   "URT010",
#'   plot_typ = "treppen"
#' )
#'
#' # Summenplot mit Treppen-Darstellung bei überlappenden Intervallen - Fehler!
#' if (FALSE) {
#'   plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr,
#'     "MUS001",
#'     plot_typ = "treppen"
#'   )
#' }
#'
#' # Verlauf von Einzelsubstanzen mit Barplots und Anzeige der Bestimmungsgrenze:
#' plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr,
#'   "URT010",
#'   id_substanz = 71
#' )
#'
#' # Verlauf mehrerer Substanzen mit Strichen:
#' plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr,
#'   "URT010",
#'   plot_typ = "striche", id_substanz = c(71, 91)
#' )
#'
#' # Verlauf Summe aller Substanzen (nicht nur Pestizide) als gruppierte Barplots
#' plot_misch_verlauf(mvdaten_beispiel_mvwizr, regulierungen_mvwizr,
#'   "URT010",
#'   plot_typ = "barplot_gruppen", zulassungstyp = "Alle",
#'   plot_parametergruppe = "PARAMETERGRUPPE"
#' )
plot_misch_verlauf <- function(mv_daten,
                               regulierungen = NULL,
                               stationscode,
                               jahr = NULL,
                               id_substanz = NULL,
                               zulassungstyp = "[BP]",
                               plot_typ = c("barplot", "treppen", "kombiniert", "striche", "barplot_gruppen"),
                               plot_bg = TRUE,
                               bg_typ = "minmax",
                               plot_parametergruppe = "") {
  plot_typ <- match.arg(plot_typ)

  # Falls NULL, verwenden wir die gebundelten Regulierungs-Informationen
  regulierungen <- regulierungen %||% mvwizr::regulierungen_mvwizr

  # Unterschiedliche Plottitel je nach ausgewähltem Zulassungstyp
  plot_titel <- switch(zulassungstyp,
    "[BP]" = "Pestizide Summen-Konzentration",
    "[B]" = "Biozide Summen-Konzentration",
    "[P]" = "PSM Summen-Konzentration",
    "Alle" = "Summen-Konzentrationen",
    sprintf("Summen-Konzentrationen f\u00fcr %s", zulassungstyp)
  )

  # Join der MV-Daten mit der Regulierungstabelle
  mv_daten <- dplyr::left_join(mv_daten, regulierungen, by = "ID_Substanz")

  # Stichproben (kein Enddatum) werden ausgeschlossen. NB: Die Angaben "S" (für Stichprobe) in der Spalte PROBEARTID ist nicht zuverlässig.
  mv_daten <- dplyr::filter(mv_daten, .data$CODE %in% .env$stationscode, !is.na(.data$ENDEPROBENAHME))

  # Verwende benutzerdefinierte Subklasse für Condition-Objekt für präzisere Tests
  if (nrow(mv_daten) == 0) {
    cli::cli_abort(message = "Keine Mischprobendaten f\u00fcr Station {stationscode} gefunden.", class = "mvwizr_error_empty_dataset")
  }

  stationsname <- unique(mv_daten$STANDORT)

  # NB: Alle Umlaute/Unicode-Spezialzeichen als Escape-Sequenz geschrieben, weil sonst R CMD CHECK eine Warnung macht. Kann automatisch mit dem Prefixer Package/Addin umgewandelt werden.
  plot_titel <- paste(plot_titel, "f\u00fcr Station", stationsname)

  # Überprüfen, ob Wert für plot_parametergruppe valid ist
  if (!is.null(plot_parametergruppe) && plot_parametergruppe %in% names(mv_daten) && is.null(id_substanz) && plot_typ == "barplot_gruppen") {
    plot_parametergruppe <- dplyr::sym(plot_parametergruppe)
  } else {
    plot_parametergruppe <- NULL
  }

  # Extrahiere Jahr und Intervalldauer pro Probe (in Tagen)
  mv_daten <- dplyr::mutate(
    mv_daten,
    Jahr = lubridate::year(.data$BEGINNPROBENAHME),
    Tage = difftime(.data$ENDEPROBENAHME, .data$BEGINNPROBENAHME, units = "days")
  )

  # Überprüfen, ob Eingabewert für id_substanz numerisch ist
  id_substanz <- if (!is.numeric(id_substanz) &&
    !length(id_substanz) == 1) {
    NULL
  } else {
    as.integer(id_substanz)
  }

  # Filtern nach id_substanz, falls angegeben
  if (!is.null(id_substanz)) {
    mv_daten <- dplyr::filter(mv_daten, .data$ID_Substanz %in% .env$id_substanz)

    if (nrow(mv_daten) == 0) {
      cli::cli_abort(message = "Keine Mischprobendaten f\u00fcr Station {stationscode} und f\u00fcr Substanz(en) mit ID {id_substanz} gefunden.", class = "mvwizr_error_empty_dataset")
    }
  }

  # Filtern nach Jahr (aus Beginn der Probenahme), falls angegeben
  if (!is.null(jahr)) {
    mv_daten <- dplyr::filter(mv_daten, .data$Jahr %in% .env$jahr)

    if (nrow(mv_daten) == 0) {
      cli::cli_abort(message = "Keine Mischprobendaten f\u00fcr Station {stationscode} und f\u00fcr Substanz(en) mit ID {id_substanz} f\u00fcr Jahr {jahr} gefunden.", class = "mvwizr_error_empty_dataset")
    }
  }

  date_axis <- setup_date_axis(mv_daten)
  plot_limits <- date_axis$limits
  date_breaks <- date_axis$breaks
  markierungen_jahre <- date_axis$markierungen

  # Aufsummierung für Summenplots: 1. Filtern nach Zulassungstyp (nicht nötig bei einzelnen/mehreren Substanzen, weil dort Auswahl via ID_Substanz)
  # 2. Gruppieren und aufsummieren der Konzentrationen pro Station und spezifische Probe.
  # 3. Neue eindeutige ID (UID) für jede Probe -> Wichtig für Treppenplots
  if (is.null(id_substanz)) {
    if (!zulassungstyp == "Alle") {
      # Nur filtern, wenn zulassungstyp nicht "Alle" ist, damit NA-Einträge nicht rusfliegen

      mv_daten <- mv_daten |>
        dplyr::filter(stringr::str_detect(.data[["Informationen Recht"]], .env$zulassungstyp))

      if (nrow(mv_daten) == 0) {
        cli::cli_abort(message = "Keine Mischprobendaten f\u00fcr Station {stationscode} und Zulassungstyp {zulassungstyp} gefunden.", class = "mvwizr_error_empty_dataset")
      }
    }

    mv_daten <- mv_daten |>
      dplyr::group_by(
        .data$CODE,
        .data$STANDORT,
        .data$NAME,
        .data$Jahr,
        .data$Tage,
        .data$BEGINNPROBENAHME,
        .data$ENDEPROBENAHME,
        {{ plot_parametergruppe }} # rlang indirection, siehe https://dplyr.tidyverse.org/articles/programming.html
      ) |>
      dplyr::summarise(WERT_NUM = sum(.data$WERT_NUM)) |>
      dplyr::mutate(gruppe_ymax = cumsum(.data$WERT_NUM), gruppe_ymin = .data$gruppe_ymax - .data$WERT_NUM) |>
      dplyr::ungroup() |>
      dplyr::arrange(.data$BEGINNPROBENAHME) |>
      dplyr::mutate(UID = dplyr::row_number(), .before = 1)
  }

  # Für Treppenplots müssen wir Beginn- und Enddatum der Probe als einzelne Punkte abbilden (daher Treppenplot). Dazu müssen wir die Differenz des Startdatums der nächsten Probe zum Enddatum der aktuellen Probe bilden. Damit dann im Plot die Reihenfolge stimmt, addieren wir bei BEGINNPROBENAHME jeweils 1 Sekunde.
  # Ausserdem entfernen wir alle kurzen Messungen (<10 Tage). Dies ist nicht dringend notwendig, reduziert aber die Wahrscheinlichkeit von überlappenden Intervallen.
  if (plot_typ == "treppen" || plot_typ == "kombiniert") {
    mv_daten_treppen <- mv_daten |>
      tidyr::pivot_longer(
        c("BEGINNPROBENAHME", "ENDEPROBENAHME"),
        names_to = "Datum_Typ",
        values_to = "Datum"
      ) |>
      dplyr::mutate(
        Datum = dplyr::if_else(
          .data$Datum_Typ == "BEGINNPROBENAHME",
          .data$Datum + 1,
          .data$Datum
        )
      ) |>
      dplyr::filter(.data$Tage >= 10)

    if (nrow(mv_daten_treppen) == 0) {
      cli::cli_abort(message = "Keine Mischprobendaten f\u00fcr Station {stationscode} f\u00fcr Treppen/Kombi-plot (nur Mischproben >= 10 Tage) gefunden.", class = "mvwizr_error_empty_dataset")
    }

    # Bei summenplots müssen wir für den nächsten Schritt nur nach UID und Datum sortieren, sonst auch zuerst noch nach Substanz
    if (purrr::is_empty(id_substanz)) {
      mv_daten_treppen <- dplyr::arrange(
        mv_daten_treppen,
        .data$UID,
        .data$Datum,
        .data$Datum_Typ
      )
    } else {
      mv_daten_treppen <- dplyr::arrange(
        mv_daten_treppen,
        .data$ID_Substanz,
        .data$UID,
        .data$Datum,
        .data$Datum_Typ
      ) |>
        dplyr::group_by(.data$ID_Substanz)
    }

    # Im geordneten Dataframe können wir nun die Datumsdifferenz zwischen Proben berechnen. Für die Treppenplots identifizieren wir nun Proben, bei denen eine Lücke besteht, d.h. bei denen die nächste Probe nicht unmittelbar anknüpft. Dort müssen wir NA-Werte einfügen, damit die Treppe unterbricht (und nicht Punkte über einen Messunterbruch verbindet).
    mv_daten_treppen <- mv_daten_treppen |>
      dplyr::mutate(
        Datum_diff = dplyr::lead(.data$Datum) - .data$Datum,
        Add_NA = dplyr::if_else(
          .data$Datum_Typ == "ENDEPROBENAHME" &
            .data$Datum_diff > 1,
          .data$Datum + 86400,
          lubridate::NA_POSIXct_
        )
      ) |>
      dplyr::ungroup()

    # Bei den Summenplots können wir die NA-Werte direkt einsetzen, sonst müssen wir dies pro Substanz tun.
    if (purrr::is_empty(id_substanz)) {
      NA_daten <- dplyr::tibble(
        Datum = stats::na.omit(unique(mv_daten_treppen$Add_NA)),
        WERT_NUM = NA_real_
      )
    } else {
      NA_daten <- mv_daten_treppen |>
        dplyr::distinct(.data$BAFU_Bez_DE, .data$Add_NA) |>
        dplyr::filter(!is.na(.data$Add_NA)) |>
        dplyr::mutate(Datum = .data$Add_NA, WERT_NUM = NA_real_)
    }

    # Negative Zeitdifferenzen zeigen überlappende Intervalle an...
    neg_diff <- stats::na.omit(mv_daten_treppen[mv_daten_treppen$Datum_diff < 0, "Datum"])[[1]]

    # ...überlappende Intervalle können nicht trivial entfernt werden, resp. sollten manuell entfernt werden. Die Fehlermeldung weist deshalb darauf hin, wo sich diese Überlappungen befinden.
    if (any(mv_daten_treppen$Datum_diff < 0, na.rm = TRUE)) {
      cli::cli_abort(
        message = c(
          "F\u00fcr Treppenplots d\u00fcrfen keine Daten mit \u00fcberlappenden Intervallen existieren.",
          "x" = "\u00dcberlappungen gefunden f\u00fcr folgende Startdaten: {neg_diff}"
        ),
        class = "mvwizr_error_treppen_ueberlapp"
      )
    }

    # Anfügen der NA-Werte an unsere Daten für den Treppenplot
    mv_daten_treppen <- dplyr::bind_rows(mv_daten_treppen, NA_daten)
  }

  # Für längere Verläufe ist ein Label pro Monat zu dicht. Zudem stellen wir mit der Funktion lagged_labels_jahr() sicher, dass jeweils der erste Monat pro Jahr mit dem aktuellen Jahr ergänzt wird.
  # Auf der linken Seite setzen wir expansion = 0, auf der rechten Seite erlauben wir ein bisschen Platz, um Proben am Jahresende plotten zu können
  plot_basis <- ggplot2::ggplot() +
    ggplot2::scale_x_datetime(
      "",
      breaks = date_breaks,
      date_minor_breaks = "1 month",
      labels = lagged_labels_jahr,
      limits = plot_limits,
      expand = ggplot2::expansion(mult = c(0, 0.01))
    ) +
    ggplot2::scale_y_continuous("Konzentration (\u00b5g/L)") +
    ggplot2::geom_vline(ggplot2::aes(xintercept = !!markierungen_jahre)) +
    plot_theme_proto() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = ggplot2::rel(1.1), colour = "black"),
      plot.margin = ggplot2::unit(c(0.25, 0.5, 0, 0.25), "inches")
    )

  # Hinzufügen der jeweiligen geoms zum Basisplot
  if (is.null(id_substanz)) {
    if (plot_typ == "treppen") {
      plot_final <- plot_basis +
        ggplot2::geom_line(
          ggplot2::aes(x = .data$Datum, y = .data$WERT_NUM),
          mv_daten_treppen
        )
    } else if (plot_typ == "barplot") {
      # Wir verwenden geom_rect und nicht geom_col oder geom_bar, weil wir so mehr Kontrolle über die Positionierung auf der x-Achse haben.
      # NB: Es wäre möglich, bei geom_col eine variable "width" einzustellen (in aes()); aber diese Option ist undokumentiert und erzeugt eine Warnung => könnte entfernt werden.
      plot_final <- plot_basis +
        ggplot2::geom_rect(
          ggplot2::aes(
            xmin = .data$BEGINNPROBENAHME,
            xmax = .data$ENDEPROBENAHME,
            ymin = 0,
            ymax = .data$WERT_NUM
          ),
          mv_daten
        )
    } else if (plot_typ == "kombiniert") {
      plot_final <- plot_basis +
        ggplot2::geom_rect(
          ggplot2::aes(
            xmin = .data$BEGINNPROBENAHME,
            xmax = .data$ENDEPROBENAHME,
            ymin = 0,
            ymax = .data$WERT_NUM
          ),
          mv_daten,
          fill = "#02A4D399"
        ) +
        ggplot2::geom_line(
          ggplot2::aes(x = .data$Datum, y = .data$WERT_NUM),
          mv_daten_treppen,
          colour = "red"
        )
    } else if (plot_typ == "barplot_gruppen") {
      plot_final <- plot_basis +
        ggplot2::geom_rect(
          ggplot2::aes(
            xmin = .data$BEGINNPROBENAHME,
            xmax = .data$ENDEPROBENAHME,
            ymin = .data$gruppe_ymin,
            ymax = .data$gruppe_ymax,
            fill = {{ plot_parametergruppe }}
          ),
          mv_daten
        ) +
        ggplot2::scale_fill_brewer(palette = "Set1") + # Funktioniert mit maximal 9 Kategorien/Farben
        ggplot2::theme(
          legend.title = ggplot2::element_blank(),
          legend.justification.bottom = "left"
        )
    } else {
      cli::cli_abort(message = c("Ung\u00fcltiger Plottyp f\u00fcr Summenverlauf", "i" = "G\u00fcltige Werte f\u00fcr `plot_typ`: `barplot`, `kombiniert`, `treppen`"), class = "mvwizr_error_plottyp_ungueltig")
    }

    plot_final <- plot_final + ggplot2::ggtitle(plot_titel)
  } else if (length(id_substanz) == 1) {
    bafu_name <- unique(mv_daten$BAFU_Bez_DE)

    plot_titel <- paste0(
      "Konzentrationsverlauf f\u00fcr ",
      bafu_name,
      " (ID = ",
      id_substanz,
      "; Station: ",
      stationsname,
      ")"
    )

    if (plot_typ == "striche") {
      plot_final <- plot_basis +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = .data$BEGINNPROBENAHME,
            y = .data$WERT_NUM,
            xend = .data$ENDEPROBENAHME,
            yend = .data$WERT_NUM
          ),
          mv_daten,
          linewidth = 1
        ) +
        ggplot2::ggtitle(plot_titel)
    } else if (plot_typ == "barplot") {
      plot_final <- plot_basis +
        ggplot2::geom_rect(
          ggplot2::aes(
            xmin = .data$BEGINNPROBENAHME,
            xmax = .data$ENDEPROBENAHME,
            ymin = 0,
            ymax = .data$WERT_NUM
          ),
          mv_daten
        )
    } else if (plot_typ == "treppen") {
      plot_final <- plot_basis +
        ggplot2::geom_line(
          ggplot2::aes(x = .data$Datum, y = .data$WERT_NUM),
          mv_daten_treppen
        )
    } else {
      cli::cli_abort(message = c("Ung\u00fcltiger Plottyp f\u00fcr Verlauf von Einzelsubstanz", "i" = "G\u00fcltige Werte f\u00fcr `plot_typ`: `barplot`, `striche`, `treppen`"), , class = "mvwizr_error_plottyp_ungueltig")
    }

    # Falls Bestimmungsgrenzen in den Daten sind und diese nicht NA sind, werden diese beim Einzelsubstanz-Plot hinzugefügt. Achtung: Standardmässig werden die BG als Minima und Maxima beim Einlesen aufgrund der eingelesenen Daten bestimmt - falls dort sehr unterschiedliche BG gefunden werden, wird dies hier wiedergegeben.
    if (plot_bg && "BG_min" %in% names(mv_daten) &&
      "BG_max" %in% names(mv_daten) && bg_typ == "minmax") {
      BG_min <- unique(mv_daten$BG_min)
      BG_max <- unique(mv_daten$BG_max)
      if (!is.na(BG_min) || !is.na(BG_max)) {
        plot_final <- plot_final +
          ggplot2::geom_hline(
            ggplot2::aes(yintercept = !!BG_min, colour = "Min. Bestimmungsgrenze"),
            linetype = 3,
            linewidth = 1
          ) +
          ggplot2::geom_hline(
            ggplot2::aes(yintercept = !!BG_max, colour = "Max. Bestimmungsgrenze"),
            linetype = 3,
            linewidth = 1
          ) +
          ggplot2::scale_colour_manual(
            "Bestimmungsgrenzen",
            values = c(
              "Min. Bestimmungsgrenze" = "dodgerblue",
              "Max. Bestimmungsgrenze" = "firebrick"
            )
          )
      } else {
        cli::cli_warn("Keine minimalen/maximalen Bestimmungsgrenzen f\u00fcr Substanz mit ID {id_substanz} gefunden.")
      }
    }

    if (plot_bg && bg_typ == "effektiv") {
      if ("Bestimmungsgrenze" %in% names(mv_daten)) {
        plot_final <- plot_final +
          ggplot2::geom_rect(
            ggplot2::aes(
              xmin = .data$BEGINNPROBENAHME,
              xmax = .data$ENDEPROBENAHME,
              ymin = 0,
              ymax = .data$Bestimmungsgrenze
            ),
            mv_daten,
            fill = "dodgerblue", alpha = 0.2
          )
      } else {
        cli::cli_warn("Spalte 'Bestimmungsgrenze' nicht in Daten gefunden")
      }
    }

    plot_final <- plot_final + ggplot2::ggtitle(plot_titel)
  } else if (length(id_substanz) > 1) {
    # Plots für mehrere Substanzen
    plot_titel <- paste0(
      "Konzentrationsverlauf f\u00fcr mehrere Substanzen (Station: ",
      stationsname,
      ")"
    )

    if (plot_typ == "striche") {
      plot_final <- plot_basis +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = .data$BEGINNPROBENAHME,
            y = .data$WERT_NUM,
            xend = .data$ENDEPROBENAHME,
            yend = .data$WERT_NUM,
            colour = .data$BAFU_Bez_DE
          ),
          mv_daten,
          linewidth = 1
        ) +
        # Maximal 9 Farben möglich
        ggplot2::scale_colour_brewer("Substanzname\n(BAFU)", palette = "Set1") +
        ggplot2::ggtitle(plot_titel)
    } else if (plot_typ == "treppen") {
      plot_final <- plot_basis +
        ggplot2::geom_line(
          ggplot2::aes(
            x = .data$Datum,
            y = .data$WERT_NUM,
            colour = .data$BAFU_Bez_DE
          ),
          mv_daten_treppen
        ) +
        ggplot2::scale_colour_brewer("Substanzname\n(BAFU)", palette = "Set1") +
        ggplot2::ggtitle(plot_titel)
    } else {
      cli::cli_abort(message = c("Ung\u00fcltiger Plottyp f\u00fcr Verlauf von mehreren Substanzen", "i" = "G\u00fcltige Werte f\u00fcr `plot_typ`: `striche`, `treppen`"), class = "mvwizr_error_plottyp_ungueltig")
    }
    plot_final <- plot_final + ggplot2::ggtitle(plot_titel)
  }

  plot_final
}

## Ue GSchV ####

#' GSchV-Überschreitungen in Mischproben plotten
#'
#' Plottet Konzentrationsüberschreitungen von Stoffen, für die in der GSchV entweder ein ökotoxikologisch begründeter Wert vorliegt oder ein allgemeiner Grenzwert (0.1 µg/l).
#'
#' @inheritParams plot_misch_mixtox_verlauf
#' @param plot_typ Legt fest, für welche Art von Überschreitungen eine Auswertung vorgenommen werden soll. Mögliche Werte:
#' \itemize{
#'   \item `"andauernd"`: Berücksichtigt nur Stoffe mit einem spezifischen andauernden Grenzwert in der GSchV. Nur Proben mit einer Dauer von >= 10 Tagen werden verwendet und unter Verwendung des CQK beurteilt.
#'   \item `"kurzzeitig"`: Berücksichtigt nur Stoffe mit einem spezifischen akuten Grenzwert in der GSchV. Berücksichtigt alle Proben (ausser Stichproben) und verwendet das AQK zur Beurteilung.
#'   \item `"allgemein"`:  Berücksichtigt nur Stoffe, für die kein ökotoxikologisch begründeter Wert in der GSchV vorliegt. Beurteilung mittels allgemeinem Grenzwert (0.1 µg/l)
#' }
#'
#' @return ggplot2 Plot-Objekt
#' @export
#'
#' @examples
#' # Andauernde Überschreitungen (und Unterschreitungen) für die Jahre 2019 und 2020 plotten
#' plot_misch_ue(rq_ue_beispiel_mvwizr, "URT010", jahr = c(2019, 2020))
#'
#' # Allgemeine Überschreitungen für 2019 anzeigen
#' plot_misch_ue(rq_ue_beispiel_mvwizr, "URT010", plot_typ = "allgemein", jahr = 2019)
#'
#' # Kurzzeitige Überschreitungen für Station MUS001 für gesamte Zeitdauer in Daten anzeigen
#' plot_misch_ue(rq_ue_beispiel_mvwizr, "MUS001", plot_typ = "kurzzeitig")
plot_misch_ue <- function(rq_ue_daten,
                          stationscode,
                          jahr = NULL,
                          plot_typ = c("andauernd", "allgemein", "kurzzeitig")) {
  # Für Ue-Plots sind nur Stoffe relevant, die in GSchV Anh.2 geregelt sind (organische Pestizide)
  plot_typ <- match.arg(plot_typ)
  rq_ue_daten <- rq_ue_daten |>
    dplyr::filter(.data$CODE %in% .env$stationscode, .data$GSCHV %in% c(1, 2), !is.na(.data$ENDEPROBENAHME)) |>
    dplyr::select(dplyr::all_of(c("CODE", "STANDORT", "BEGINNPROBENAHME", "ENDEPROBENAHME", "ID_Substanz", "BAFU_Bez_DE", "Jahr", "Tage", "GSCHV", "AQK", "CQK")), dplyr::any_of("BG_max"), dplyr::starts_with("Ue"))

  # Verwende benutzerdefinierte Subklasse für Condition-Objekt für präzisere Tests
  if (nrow(rq_ue_daten) == 0) {
    cli::cli_abort(message = "Keine Mischprobendaten f\u00fcr Station {stationscode} gefunden.", class = "mvwizr_error_empty_dataset")
  }

  stationsname <- unique(rq_ue_daten$STANDORT)

  # Falls kein Jahr angegeben wird, werden alle Daten geplottet
  jahre_daten <- unique(lubridate::year(rq_ue_daten$BEGINNPROBENAHME))
  if (purrr::is_null(jahr)) {
    jahr <- jahre_daten
  }

  if ("BG_max" %in% names(rq_ue_daten)) {
    switch_cap <- "\u00b0 Die Bestimmungsgrenze liegt m\u00f6glicherweise \u00fcber der numerischen Anforderung,\neine Verunreinigung kann daher nicht sicher ausgeschlossen werden f\u00fcr diese Substanz."
  } else {
    rq_ue_daten$BG_max <- 0
    # waiver() bewirkt, dass ggplot einen Standardwert verwendet/berechnet
    switch_cap <- ggplot2::waiver()
  }

  rq_ue_daten_station <- rq_ue_daten |>
    dplyr::filter(.data$Jahr %in% .env$jahr)

  # Für die Beschriftung resp. Zusammenfassung pro Substanz Aggregation der Überschreitungen nach Substanz
  Ue_pro_Substanz <- rq_ue_daten_station |>
    dplyr::group_by(.data$BAFU_Bez_DE) |>
    dplyr::summarise(
      AnyUe_anh = any(.data$Ue_anhaltend),
      AnyUe_kurz = any(.data$Ue_kurzzeitig),
      AnyUe_allg = any(.data$Ue_generisch)
    ) |>
    dplyr::ungroup()

  # Je nach plot_typ sollen unterschiedliche Daten verwendet werden. Daher sind im switch statement pro plot_typ die Symbole hinterlegt, nach denen später z.B. gefiltert wird.
  switch(plot_typ,
    "andauernd" = {
      AnyUe <- dplyr::sym("AnyUe_anh")
      Ue_GSchV_fill <- dplyr::sym("Ue_anhaltend")
      # Für andauernde Überschreitungen berücksichtigen wir nur Proben >= 10 Tage
      mv_daten_Ue <- rq_ue_daten_station |> dplyr::filter(
        .data$Tage >= 10,
        .data$GSCHV == 1,
        !is.na(.data$Ue_anhaltend)
      )
      titel_plot <- "Andauernde Verunreinigungen gem\u00e4ss GSchV"
    },
    "kurzzeitig" = {
      AnyUe <- dplyr::sym("AnyUe_kurz")
      Ue_GSchV_fill <- dplyr::sym("Ue_kurzzeitig")
      mv_daten_Ue <- rq_ue_daten_station |> dplyr::filter(.data$GSCHV == 1, !is.na(.data$Ue_kurzzeitig))
      titel_plot <- "Kurzzeitige Verunreinigungen gem\u00e4ss GSchV"
    },
    "allgemein" = {
      # Betrifft nur Substanzen ohne einen spezifischen Grenzwert in der GSchV
      AnyUe <- dplyr::sym("AnyUe_allg")
      Ue_GSchV_fill <- dplyr::sym("Ue_generisch")
      mv_daten_Ue <- rq_ue_daten_station |> dplyr::filter(.data$GSCHV == 2)
      titel_plot <- "Allgemeine Verunreinigungen gem\u00e4ss GSchV"
    }
  )

  # Verwende benutzerdefinierte Subklasse für Condition-Objekt für präzisere Tests
  if (nrow(mv_daten_Ue) == 0) {
    cli::cli_abort(message = "Keine Mischprobendaten f\u00fcr Station {stationscode} gefunden und f\u00fcr Plot-Typ gefunden.", class = "mvwizr_error_empty_dataset")
  }

  # Mit den Informationen zur Überschreitung pro Substanz können die Substanznamen im Plot als HTML/Markdown formatiert werden (rot/fett).
  # Zudem wird eine Warnung hinzugefügt, falls die BG höher ist als der Grenzwert
  mv_daten_plot <- mv_daten_Ue |>
    dplyr::left_join(Ue_pro_Substanz, by = "BAFU_Bez_DE") |>
    dplyr::arrange(.data$BAFU_Bez_DE, .locale = get_lang()) |>
    dplyr::mutate(
      BG_warnung = dplyr::if_else(
        !is.na(.data$BG_max) &
          (
            .data$BG_max > .data$CQK |
              .data$BG_max > .data$AQK
          ),
        "\u00b0",
        ""
      ),
      BG_warnung = tidyr::replace_na(.data$BG_warnung, ""),
      BEZEICHNUNG_md = dplyr::if_else(
        !!AnyUe,
        paste(
          "<b style = 'color: red;'>",
          paste0(.data$BAFU_Bez_DE, .data$BG_warnung),
          "</b>"
        ),
        .data$BAFU_Bez_DE
      ),
      BEZEICHNUNG_md = forcats::fct(.data$BEZEICHNUNG_md),
      # Substanzbezeichnung als Faktor als Integer-Wert für y-Skala
      Bez_fct_int = as.integer(.data$BEZEICHNUNG_md),
      # Anfangs- und Endkoordinaten für geom_rect auf y-Achse
      plot_ymin = .data$Bez_fct_int - 0.5,
      plot_ymax = .data$Bez_fct_int + 0.5
    )

  optimised_plot_df <- mv_daten_plot |>
    dplyr::group_by(.data$ID_Substanz, .data$BAFU_Bez_DE, .data$BEZEICHNUNG_md, .data$Bez_fct_int, .data$plot_ymin, .data$plot_ymax, !!Ue_GSchV_fill) |>
    dplyr::arrange(.data$ID_Substanz, .data$BEGINNPROBENAHME, .data$ENDEPROBENAHME) |>
    dplyr::mutate(Overlap = difftime(.data$BEGINNPROBENAHME, dplyr::lag(.data$ENDEPROBENAHME)), Overlap = dplyr::if_else(.data$Overlap < 86400 | is.na(.data$Overlap), lubridate::duration(0, units = "sec"), .data$Overlap), cum_group = cumsum(.data$Overlap)) |>
    dplyr::group_by(.data$ID_Substanz, .data$BAFU_Bez_DE, .data$BEZEICHNUNG_md, .data$Bez_fct_int, .data$plot_ymin, .data$plot_ymax, !!Ue_GSchV_fill, .data$cum_group) |>
    dplyr::mutate(BEGINNPROBENAHME = min(.data$BEGINNPROBENAHME), ENDEPROBENAHME = max(.data$ENDEPROBENAHME)) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(c("ID_Substanz", "BAFU_Bez_DE", "BEZEICHNUNG_md", "Bez_fct_int", "BEGINNPROBENAHME", "ENDEPROBENAHME", "plot_ymin", "plot_ymax")), !!Ue_GSchV_fill) |>
    dplyr::distinct()

  plot_limits <- c(
    lubridate::as_datetime(paste0(min(
      mv_daten_plot$Jahr
    ), "-01-01")),
    max(mv_daten_plot$ENDEPROBENAHME, na.rm = TRUE)
  )

  date_breaks <- seq(plot_limits[1], lubridate::as_datetime(paste0(max(
    lubridate::year(mv_daten_plot$BEGINNPROBENAHME)
  ), "-12-31")), by = "2 months")

  markierungen_jahre <- lubridate::as_datetime(paste0(unique(mv_daten_plot$Jahr), "-12-31"))

  # Teiltransparenter Wert für blaue Färbung - damit überschneidende Intervalle sichtbar werden
  main_plot <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$BEGINNPROBENAHME,
        xmax = .data$ENDEPROBENAHME,
        ymin = .data$plot_ymin,
        ymax = .data$plot_ymax,
        fill = !!Ue_GSchV_fill
      ),
      optimised_plot_df
    ) +
    ggplot2::scale_fill_manual(
      name = "\u00dcberschreitung GSchV",
      breaks = c(FALSE, TRUE),
      values = c("#1e90ff22", "firebrick"),
      labels = c("Nein", "Ja")
    ) +
    ggplot2::scale_x_datetime(
      "",
      breaks = date_breaks,
      date_minor_breaks = "1 month",
      labels = lagged_labels_jahr,
      limits = plot_limits,
      expand = ggplot2::expansion(mult = c(0, 0.01))
    ) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = !!markierungen_jahre)) +
    # Reverse, damit y-Skala z->A läuft (A oben)
    ggplot2::scale_y_reverse(
      breaks = optimised_plot_df$Bez_fct_int,
      labels = optimised_plot_df$BEZEICHNUNG_md,
      expand = c(0, 0)
    ) +
    # Plotbasis-theme
    plot_theme_proto() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggtext::element_markdown(colour = "black"),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      # Keine Abstände von Legende - für Ausrichtung der Legende wichtig
      legend.margin = ggplot2::margin(0, 0, 0, 0)
    )

  # Für summary strip: Nur Überschreitungen pro Probe (ohne Aufschlüsselung nach Substanz) behalten
  summary_data <- optimised_plot_df |>
    dplyr::distinct(.data$BEGINNPROBENAHME,
      .data$ENDEPROBENAHME,
      !!Ue_GSchV_fill,
      .keep_all = TRUE
    ) |>
    dplyr::filter({{ Ue_GSchV_fill }})

  # Nur ein Eintrag auf y-Achse: break manuell auf 1 gesetzt und Label manuell gesetzt.
  summary_plot <- ggplot2::ggplot() +
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = .data$BEGINNPROBENAHME,
        xmax = .data$ENDEPROBENAHME,
        ymin = 0.5,
        ymax = 1.5,
        fill = !!Ue_GSchV_fill
      ),
      summary_data,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(
      breaks = c(FALSE, TRUE),
      values = c("#1e90ff22", "firebrick"),
      labels = c("Nein", "Ja")
    ) +
    ggplot2::scale_x_datetime(
      "",
      breaks = date_breaks,
      date_minor_breaks = "1 month",
      labels = lagged_labels_jahr,
      limits = plot_limits,
      expand = ggplot2::expansion(mult = c(0, 0.01))
    ) +
    ggplot2::scale_y_continuous(
      breaks = 1,
      labels = "Alle Substanzen",
      expand = c(0, 0)
    ) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = !!markierungen_jahre)) +
    plot_theme_proto() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggtext::element_markdown(colour = "black"),
      axis.text.x = ggplot2::element_text(colour = "black")
    )

  # Patchwork layout mit fixem Verhältnis 50:1 (Hauptplot:Summary strip). Zusätzlich werden Legenden gesammelt und ein Untertitel und eine Caption können angebracht werden.
  belastungen_plot <- main_plot / summary_plot +
    patchwork::plot_layout(guides = "collect", heights = c(50, 1)) +
    patchwork::plot_annotation(
      title = titel_plot,
      subtitle = paste("Auswertung gem\u00e4ss GSchV Anhang 2 f\u00fcr Station", stationsname),
      caption = switch_cap
    ) & ggplot2::theme(
    legend.position = "bottom",
    # Caption und Legende links ausgerichtet
    plot.caption = ggplot2::element_text(hjust = 0),
    legend.justification.bottom = "left",
    legend.justification.inside = c(1, 1),
    legend.location = "plot"
  )

  belastungen_plot
}

#' Aufsummierte andauernde GSchV-Überschreitungen plotten
#'
#' Zeigt die andauernden Überschreitungen pro Jahr und Station. Dafür werden nur Proben mit einer Messdauer >= 10 Tage berücksichtigt.
#'
#' @inheritParams plot_misch_mixtox_verlauf
#'
#' @return ggplot2 Plot-Objekt
#' @export
#'
#' @examples
#' plot_misch_ue_summe(rq_ue_beispiel_mvwizr)
#'
plot_misch_ue_summe <- function(rq_ue_daten,
                                stationscode = NULL,
                                jahr = NULL) {
  farben_gschv <- c("Anz_spez" = "#e51f20", "Anz_gen" = "#d99594")
  rq_ue_daten <- dplyr::filter(rq_ue_daten, !is.na(.data$ENDEPROBENAHME))

  jahre_daten <- unique(lubridate::year(rq_ue_daten$BEGINNPROBENAHME))

  if (is.null(jahr)) {
    jahr <- jahre_daten
  }

  if (is.null(stationscode)) {
    stationscode <- unique(rq_ue_daten$CODE)
  }

  # Nach Überschreitungen filtern, danach Anz. Ue pro gruppe zählen; gemäss Besprechung mit Irene Wittmer hier nur anhaltende Ue
  plot_data <- rq_ue_daten |>
    dplyr::filter(.data$CODE %in% .env$stationscode, .data$Jahr %in% .env$jahr, (.data$Ue_anhaltend | .data$Ue_generisch), .data$Tage >= 10) |>
    dplyr::group_by(.data$STANDORT, .data$Jahr) |>
    dplyr::summarise(Anz_spez = sum(.data$Ue_anhaltend, na.rm = TRUE), Anz_gen = sum(.data$Ue_generisch, na.rm = TRUE)) |>
    tidyr::pivot_longer(c("Anz_spez", "Anz_gen"), names_to = "Art_Ue", values_to = "Anzahl")

  pobj <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$Jahr, y = .data$Anzahl, fill = .data$Art_Ue)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous("", breaks = jahre_daten) +
    ggplot2::facet_wrap(~ .data$STANDORT, scales = "free", ncol = 2) +
    ggplot2::scale_fill_manual(values = farben_gschv, breaks = c("Anz_spez", "Anz_gen"), labels = c("GSchV (stoffspezifische Grenzwerte)", "GSchV 0.1 \u00b5g/l")) +
    ggplot2::ggtitle("Anzahl andauernde \u00dcberschreitungen Gew\u00e4sserschutzverordnung pro Station") +
    plot_theme_proto() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      axis.ticks.length = ggplot2::unit(.25, "cm"),
      axis.text = ggplot2::element_text(colour = "black")
    )

  pobj
}

## Ue QK ####

#' Aufsummierte QK-Überschreitungen plotten
#'
#' Zeigt die Überschreitung chronischer oder akuter Qualitätskriterien pro Jahr und Station, wobei sichtbar wird, ob es viele relevante Überschreitungen gibt von Qualitätskriterien, die nicht in der GSchV aufgenommen sind als spezifischer Grenzwert.
#'
#' @inheritParams plot_misch_mixtox_verlauf
#' @param qk Qualitätskriterium, für welches geplottet werden soll. Mögliche Werte: "chronisch" (Vorgabe) oder "akut".
#' @param detailliert Logisch (Vorgabe: `FALSE`). Soll der detaillierte VSA-Plot mit Aufspaltung nach Substanzen erstellt werden?
#' @param pattern_key_scale Skalierungsfaktor (Vorgabe: 1), der angibt, wie das Muster in den Legendeneinträgen skaliert werden soll (nur für `detailliert = TRUE`). Diesen Wert reduzieren, falls ein Muster im Plot gezeigt wird, aber nicht in der Legende.
#'
#' @return ggplot2 Plot-Objekt
#' @export
#'
#' @examples
#' # Chronische Übertretungen auswerten
#' plot_misch_ue_qk(rq_ue_beispiel_mvwizr, qk = "chronisch", detailliert = FALSE)
#'
#' # Akute Übertretungen auswerten
#' plot_misch_ue_qk(rq_ue_beispiel_mvwizr, qk = "akut", detailliert = FALSE)
#'
#' \donttest{
#' # Chronische Übertretungen detailliert auswerten (VSA-Grafik)
#' # Wird nicht automatisch getestet, weil Run zu lange dauert
#' plot_misch_ue_qk(rq_ue_beispiel_mvwizr, qk = "chronisch", detailliert = TRUE)
#' }
#'
plot_misch_ue_qk <- function(rq_ue_daten,
                             stationscode = NULL,
                             jahr = NULL,
                             qk = c("chronisch", "akut"),
                             detailliert = FALSE,
                             pattern_key_scale = 1) {
  qk <- match.arg(qk)
  rq_ue_daten <- dplyr::filter(rq_ue_daten, !is.na(.data$ENDEPROBENAHME))

  farben_qk <- c("Anz_gschv" = "#e51f20", "Anz_qk" = "#fbbf09")

  if (!is.null(jahr)) {
    rq_ue_daten <- dplyr::filter(rq_ue_daten, .data$Jahr %in% .env$jahr)
  }

  jahre_daten <- unique(rq_ue_daten$Jahr)

  if (!is.null(stationscode)) {
    rq_ue_daten <- dplyr::filter(rq_ue_daten, .data$CODE %in% .env$stationscode)
  }

  switch(qk,
    "chronisch" = {
      Ue_QK <- dplyr::sym("Ue_CQK")
      Ue_GSchV <- dplyr::sym("Ue_anhaltend")
      plot_legend_label <- "CQK"
      # Für chronisches QK Proben mit >= 10 Tage
      rq_ue_daten <- dplyr::filter(rq_ue_daten, .data$Tage >= 10, .data$Ue_CQK)
    },
    "akut" = {
      Ue_QK <- dplyr::sym("Ue_AQK")
      Ue_GSchV <- dplyr::sym("Ue_kurzzeitig")
      plot_legend_label <- "AQK"
      rq_ue_daten <- dplyr::filter(rq_ue_daten, .data$Ue_AQK)
    }
  )

  # Es handelt sich um zwei verschiedene Datenverarbeitungen und Plots, daher ab hier getrennte Logik
  if (!detailliert) {
    # Aufsummierte Darstellung ohne Details. Entweder für AQK oder CQK
    rq_ue_summary <- dplyr::group_by(rq_ue_daten, .data$STANDORT, .data$Jahr) |>
      dplyr::summarise(
        Anz_gschv = sum({{ Ue_GSchV }}, na.rm = TRUE),
        Anz_qk = sum({{ Ue_QK }}, na.rm = TRUE) - .data$Anz_gschv # Damit Überschreitungen nicht doppelt gezählt werden
      ) |>
      tidyr::pivot_longer(c("Anz_gschv", "Anz_qk"),
        names_to = "Art_Ue",
        values_to = "Anzahl"
      )

    pobj <- ggplot2::ggplot(
      rq_ue_summary,
      ggplot2::aes(
        x = .data$Jahr,
        y = .data$Anzahl,
        fill = .data$Art_Ue
      )
    ) +
      ggplot2::geom_col() +
      ggplot2::scale_x_continuous("", breaks = jahre_daten) +
      ggplot2::facet_wrap(~ .data$STANDORT, scales = "free", ncol = 2) +
      ggplot2::scale_fill_manual(
        values = farben_qk,
        breaks = c("Anz_gschv", "Anz_qk"),
        labels = c("GSchV (stoffspezifische Grenzwerte)", plot_legend_label)
      ) +
      ggplot2::ggtitle("Anzahl \u00dcberschreitungen Qualit\u00e4tskriterien pro Station") +
      plot_theme_proto() +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        axis.ticks.length = ggplot2::unit(.25, "cm"),
        axis.text = ggplot2::element_text(colour = "black")
      )
  }

  if (detailliert) {
    # Hier Gruppierung zusätzlich nach Substanz
    rq_ue_summary <- dplyr::group_by(rq_ue_daten, .data$STANDORT, .data$Jahr, .data$ID_Substanz, .data$BAFU_Bez_DE) |>
      dplyr::summarise(
        gschv = sum({{ Ue_GSchV }}, na.rm = TRUE),
        qk = sum({{ Ue_QK }}, na.rm = TRUE),
        # Damit Überschreitungen nicht doppelt gezählt werden
        qk = dplyr::if_else(.data$gschv == 0, qk, 0)
      ) |>
      tidyr::pivot_longer(c("gschv", "qk"),
        names_to = "Ue",
        values_to = "Anzahl"
      ) |>
      dplyr::filter(.data$Anzahl > 0) |>
      dplyr::ungroup() |>
      # Verwendung von Faktoren zum Sortieren, aber auch weil ggpattern Probleme hatte mit den Sonderzeichen in der BAFU_Bez_DE
      dplyr::mutate(BAFU_Bez_DE_fct = forcats::fct(.data$BAFU_Bez_DE, levels = stringr::str_sort(unique(.data$BAFU_Bez_DE), locale = get_lang())))

    anz_farben <- length(unique(rq_ue_summary$ID_Substanz))

    # Die Erstellung der Farbpalette ist probabilistisch - aber für die Snapshot-Test muss immer exakt die gleiche Farbpalette verwendet werden in jedem Durchgang
    # withr::with_seed() scoped den RNG-State, so dass keine globale Zustandsänderung entsteht
    farben_substanzen <- withr::with_seed(1537062, {
      Polychrome::createPalette(anz_farben, c("#FF0000", "#00FF00", "#0000FF"), range = c(30, 80))
    })
    # Polychrome setzt standardmässig Namen für die Farben, die wir nicht wollen
    names(farben_substanzen) <- NULL

    # Bug fix, weil Polychrome mindestens 5 Farben liefert (getestet mit Polychrome v1.5.4)
    if (anz_farben < 5) {
      farben_substanzen <- farben_substanzen[1:anz_farben]
    }
    # Nur Substanzen, die in der GSchV einen Wert haben, sollen schraffiert werden.
    substanzen <- rq_ue_summary |>
      dplyr::distinct(.data$ID_Substanz, .data$BAFU_Bez_DE, .data$BAFU_Bez_DE_fct, .data$Ue) |>
      dplyr::mutate(Farbe = .env$farben_substanzen, Muster = dplyr::if_else(.data$Ue == "gschv", "stripe", "none"))

    muster <- substanzen$Muster
    names(muster) <- substanzen$BAFU_Bez_DE_fct

    farbe <- substanzen$Farbe
    names(farbe) <- substanzen$BAFU_Bez_DE_fct

    pobj <- ggplot2::ggplot(rq_ue_summary, ggplot2::aes(
      x = .data$Jahr,
      y = .data$Anzahl,
      group = .data$Ue
    )) +
      ggpattern::geom_col_pattern(
        ggplot2::aes(
          # Wir weisen die gleiche Variable dem Muster und dem Fill zu - dadurch nur eine Legende...
          pattern = .data$BAFU_Bez_DE_fct,
          fill = .data$BAFU_Bez_DE_fct
        ),
        pattern_fill = NA,
        pattern_key_scale_factor = pattern_key_scale,
        color = "black"
      ) +
      ggplot2::scale_x_continuous("", breaks = jahre_daten) +
      # ...und setzen dann manuell unterschiedliche Muster mit den benannten Farb/Mustervektoren oben
      ggpattern::scale_pattern_manual(values = muster) +
      ggplot2::scale_fill_manual(values = farbe) +
      ggplot2::facet_grid(~ .data$STANDORT, scales = "free_y") +
      ggplot2::ggtitle("Anzahl \u00dcberschreitungen Qualit\u00e4tskriterien pro Station", subtitle = "Schraffiert: Substanz mit GSchV-Wert") +
      plot_theme_proto() +
      ggplot2::theme(
        legend.title = ggplot2::element_blank(),
        legend.position = "left",
        axis.ticks.length = ggplot2::unit(.25, "cm"),
        axis.text = ggplot2::element_text(colour = "black")
      )
  }

  pobj
}

## Ökotox ####

#' Stationsübersicht Ökotoxbeurteilung plotten
#'
#' Plottet die Ökotoxbeurteilung für Einzelstoffe (kurzzeitig oder andauernd) und Mischtoxizitäten pro Jahr und Station zusammen.
#'
#' @inheritParams plot_misch_mixtox_verlauf
#'
#' @return ggplot2 Plot-Objekt
#' @export
#'
#' @examples
#'
#' plot_misch_oekotox_uebersicht(rq_ue_beispiel_mvwizr, "URT010", 2020)
#'
#' plot_misch_oekotox_uebersicht(rq_ue_beispiel_mvwizr, "MUS001", 2020, modus = "kurzzeitig")
#'
#' plot_misch_oekotox_uebersicht(rq_ue_beispiel_mvwizr, "MUS001", 2020)
plot_misch_oekotox_uebersicht <- function(rq_ue_daten,
                                          stationscode,
                                          jahr,
                                          modus = c("andauernd", "kurzzeitig"),
                                          optin_mischtox_S = FALSE) {
  modus <- match.arg(modus)
  # Je nach Modus definieren wir gewisse Variablen resp. Symbole unterschiedlich
  modus_cfg <- get_modus_config(modus)
  switchRQ <- modus_cfg$switchRQ
  switchBeurteilung <- modus_cfg$switchBeurteilung
  switchKriterium <- modus_cfg$switchKriterium
  suffix_plot <- modus_cfg$suffix_plot
  # Bei andauernden Belastungen berücksichtigen wir nur Proben mit >=10 Tage Dauer
  if (modus == "andauernd") {
    rq_ue_daten <- dplyr::filter(rq_ue_daten, .data$Tage >= 10)
  }

  # Der Plot ist immer nur für eine Station und ein bestimmtes Jahr gedacht
  rq_data <- rq_ue_daten |>
    dplyr::filter(.data$CODE %in% .env$stationscode, .data$Jahr %in% .env$jahr, !is.na(.data$ENDEPROBENAHME))

  # Verwende benutzerdefinierte Subklasse für Condition-Objekt für präzisere Tests
  if (nrow(rq_data) == 0) {
    cli::cli_abort(message = "Keine Mischprobendaten f\u00fcr Station {stationscode} gefunden.", class = "mvwizr_error_empty_dataset")
  }

  stationsname <- unique(rq_data$STANDORT)

  # Farbskala gemäss Modul-Stufen-Konzept als globale Funktion hinterlegt
  farbskala_tox <- farbskala_bewertung_ecotox()

  # Substanz-Bezeichnung als Faktor um entlang kontinuierlicher y-Skala anordnen zu können. RQ und Beurteilung wird je nach Modus unterschiedlich gesetzt (Verweis auf Symbole oben). Zur Notation siehe: https://dplyr.tidyverse.org/articles/programming.html
  # NB: Reihenfolge geändert (d.h. zuerst Einträge ohne Risikoquotienten, dann erst Faktoren erstellen), weil sonst teilweise Lücken auf y-Achse (in denen Fällen, in denen eine Substanz im Filterschritt rausfliegt)
  rq_data_fct <- rq_data |>
    dplyr::mutate(
      RQ = {{ switchRQ }},
      Beurteilung = {{ switchBeurteilung }}
    ) |>
    # Wir entfernen Einträge ohne Risikoquotienten (= mit fehlenden QK), da diese nicht geplottet werden können
    dplyr::filter(!is.na(.data$RQ)) |>
    dplyr::mutate(
      BAFU_Bez_DE = forcats::fct(.data$BAFU_Bez_DE, levels = stringr::str_sort(unique(.data$BAFU_Bez_DE), decreasing = TRUE, locale = get_lang())),
      BEZ_NUM = as.integer(.data$BAFU_Bez_DE),
    )

  # Maximale RQ je Substanz (ohne Ties!). Gerundet auf 1 Dezimalstelle für Anzeige.
  rq_summary <- rq_data_fct |>
    dplyr::select(dplyr::all_of(c("BAFU_Bez_DE", "BEZ_NUM", "RQ", "Beurteilung"))) |>
    dplyr::group_by(.data$BAFU_Bez_DE) |>
    dplyr::slice_max(order_by = .data$RQ, n = 1, with_ties = FALSE) |>
    dplyr::mutate(RQ = round(.data$RQ, 1))

  # Anzahl Stellen der RQ-Werte für Padding mit Leerzeichen
  rq_width <- max(nchar(as.character(rq_summary$RQ)))

  # Padding mit Leerzeichen, damit Zahlen alle an Dezimalzeichen ausgerichtet werden können (mit monospaced Schrift)
  rq_summary <- rq_summary |>
    # sprintf-Notation mit Stern für Padding
    dplyr::mutate(RQ_text = sprintf("%*.1f", !!rq_width, .data$RQ))

  # Plot mit RQ-Werte pro Einzelsubstanz. Wichtig: Skalen müssen gleiche breaks und limits haben wie summary, damit die Ausrichtung der Plots stimmt.
  rq_pobj <- ggplot2::ggplot(rq_data_fct) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$BEGINNPROBENAHME, xmax = .data$ENDEPROBENAHME, ymin = .data$BEZ_NUM - 0.5, ymax = .data$BEZ_NUM + 0.5, fill = .data$Beurteilung), colour = "white", linewidth = 0.1) +
    ggplot2::scale_x_datetime("",
      date_breaks = "1 month",
      labels = lagged_labels_jahr,
      position = "top", expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(breaks = rq_data_fct$BEZ_NUM, labels = rq_data_fct$BAFU_Bez_DE, expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = farbskala_tox) +
    ggplot2::guides(fill = "none") +
    ggplot2::ggtitle("Beurteilung Einzelstoffe") +
    plot_theme_proto() +
    ggplot2::theme(
      axis.ticks.length = ggplot2::unit(.25, "cm"),
      axis.text = ggplot2::element_text(colour = "black")
    )

  # Summary Plot (einzelne Spalte) für Einzelsubstanzen mit Anzeige der maximalen RQ-Werte
  rq_summary_pobj <- ggplot2::ggplot(rq_summary) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 1, xmax = 2, ymin = .data$BEZ_NUM - 0.5, ymax = .data$BEZ_NUM + 0.5, fill = .data$Beurteilung), colour = "white", linewidth = 0.1, show.legend = FALSE) +
    ggplot2::scale_y_continuous("", breaks = rq_data_fct$BEZ_NUM, labels = rq_data_fct$BAFU_Bez_DE, position = "right", expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = farbskala_tox) +
    ggplot2::guides(fill = "none") +
    ggplot2::geom_text(
      ggplot2::aes(x = 1, y = .data$BEZ_NUM, label = .data$RQ_text),
      # hjust so angepasst, dass Zahlen "schön" ausgerichtet sind
      size = 3, vjust = 0.5, check_overlap = TRUE, hjust = -0.25, family = "mono", fontface = "bold"
    ) +
    plot_theme_proto() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )

  # Mischtoxizitätdaten - gefiltert nach aktuellem QK
  mixtox_data <- berechne_mixtox(rq_data) |>
    dplyr::filter(.data$Kriterium %in% .env$switchKriterium)

  # Falls nicht explizit gewünscht wird, dass wir secondary toxicity auch bewerten, werden diese Resultate ausgeblendet, damit keine Änderung gegenüber v1.1.0 entsteht. Falls alle Daten NA sind (=keine Stoffe mit S_chron gemessen), filtern wir die Variable auch raus
  if (!optin_mischtox_S || all(is.na(mixtox_data[mixtox_data$Ziel == "Bioakkumulation", "RQ"]))) {
    mixtox_data <- dplyr::filter(mixtox_data, .data$Ziel != "Bioakkumulation")
  }

  # Mischtoxizitätsplot analog oben
  mixtox_pobj <- ggplot2::ggplot(mixtox_data) +
    ggplot2::geom_rect(ggplot2::aes(xmin = .data$BEGINNPROBENAHME, xmax = .data$ENDEPROBENAHME, ymin = .data$Ziel_num - 0.5, ymax = .data$Ziel_num + 0.5, fill = .data$Beurteilung), colour = "white", linewidth = 0.1, show.legend = TRUE) +
    ggplot2::scale_x_datetime("",
      date_breaks = "1 month",
      labels = lagged_labels_jahr,
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous(breaks = mixtox_data$Ziel_num, labels = mixtox_data$Ziel, expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = farbskala_tox, drop = FALSE) +
    ggplot2::ggtitle("Beurteilung Mischungstoxizit\u00e4t") +
    plot_theme_proto() +
    ggplot2::theme(
      axis.ticks.length = ggplot2::unit(.25, "cm"),
      axis.text = ggplot2::element_text(colour = "black")
    )

  # Mischtoxizitäts-Summary analog oben
  mixtox_summary <- mixtox_data |>
    dplyr::select(dplyr::all_of(c("CODE", "Ziel", "Ziel_num", "Beurteilung", "RQ"))) |>
    dplyr::group_by(.data$CODE, .data$Ziel, .data$Ziel_num) |>
    dplyr::slice_max(.data$RQ, n = 1, with_ties = FALSE) |>
    dplyr::mutate(RQ = round(.data$RQ, 1))

  mixtox_rq_width <- max(nchar(as.character(stats::na.omit(mixtox_summary$RQ))))

  mixtox_summary <- dplyr::mutate(mixtox_summary, RQ_text = sprintf("%*.1f", !!mixtox_rq_width, .data$RQ))

  # Mischtoxizitäts-Summary-Plot analog oben
  mixtox_summary_pobj <- ggplot2::ggplot(mixtox_summary) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 1, xmax = 2, ymin = .data$Ziel_num - 0.5, ymax = .data$Ziel_num + 0.5, fill = .data$Beurteilung), colour = "white", linewidth = 0.1, show.legend = FALSE) +
    ggplot2::scale_y_continuous("", breaks = mixtox_summary$Ziel_num, labels = mixtox_summary$Ziel, position = "right", expand = c(0, 0)) +
    ggplot2::scale_fill_manual(values = farbskala_tox) +
    ggplot2::guides(fill = "none") +
    ggplot2::geom_text(
      ggplot2::aes(x = 1, y = .data$Ziel_num, label = .data$RQ_text),
      size = 3, vjust = 0.5, check_overlap = TRUE, hjust = -0.25, family = "mono", fontface = "bold"
    ) +
    plot_theme_proto() +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )

  # Layout, um die 4 Plots mit patchwork anzuordnen
  mixplot_layout <- c(
    patchwork::area(t = 1, l = 1, b = 28, r = 18),
    patchwork::area(t = 1, l = 19, b = 28, r = 20),
    patchwork::area(t = 29, l = 1, b = 30, r = 18),
    patchwork::area(t = 29, l = 19, b = 30, r = 20)
  )

  # Patchwork-Objekt. ggplot-theming wird für alle Plot-Objekte im patchwork integral angewandt
  mixplot_patchwork <- rq_pobj + rq_summary_pobj + mixtox_pobj + mixtox_summary_pobj + patchwork::plot_layout(guides = "collect", design = mixplot_layout) +
    patchwork::plot_annotation(
      title = paste0("Stations\u00fcbersicht \u00d6kotoxbeurteilung", suffix_plot),
      subtitle = paste("Auswertung f\u00fcr Station", stationsname, "f\u00fcr das Jahr", jahr),
    ) & ggplot2::theme(
    legend.position = "bottom",
    plot.caption = ggplot2::element_text(hjust = 0),
    legend.justification.bottom = "left",
    legend.justification.inside = c(1, 1),
    legend.location = "plot",
    legend.margin = ggplot2::margin(0, 0, 0, 0)
  )

  mixplot_patchwork
}

#' Verlauf von Mischungstoxizitäten plotten
#'
#' Zeigt den Verlauf der Mischungstoxizitäten pro Jahr (fortlaufend möglich) und Stationen.
#'
#' @param rq_ue_daten Dataframe mit Output der Funktion `berechne_rq_ue()`
#' @param stationscode Station, für welche der Plot erstellt werden soll.
#' @param jahr Jahr, für welches der Überschreitungsplot erstellt werden soll.
#' @param modus
#' \itemize{
#'   \item `"andauernd"`: Berücksichtigt nur Stoffe mit einem spezifischen andauernden Grenzwert in der GSchV. Nur Proben mit einer Dauer von >= 10 Tagen werden verwendet und unter Verwendung des CQK beurteilt.
#'   \item `"kurzzeitig"`: Berücksichtigt nur Stoffe mit einem spezifischen akuten Grenzwert in der GSchV. Berücksichtigt alle Proben (ausser Stichproben) und verwendet das AQK zur Beurteilung.
#' }
#' @param plot_zusammenfassung Entweder `NULL` (Vorgabe), "stichproben" oder "mischproben". Falls einer der letzeren beiden, wird pro Jahr und nicht pro Monat aggregiert (entweder nur für Stichproben oder nur für Mischproben). Für Stichproben muss der `modus` auf "kurzzeitig" gesetzt werden.
#' @param optin_mischtox_S Logisch (Vorgabe: `FALSE`). Ab v1.2 unterstützt mvwizr auch die Anzeige der Bioakkumulation / Secondary toxicity (`S_chron`). Falls `TRUE`, wird eine vierte Zeile bei den Mischtoxizitäten angezeigt, falls sie in den Daten vorhanden ist. Falls `FALSE`, wird sie nicht angezeigt.
#'
#' @return ggplot2 Plot-Objekt
#' @export
#'
#' @examples
#' # Ausführlicher Verlauf für andauernde Belastungen
#' plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "andauernd")
#'
#' # Ausführlicher Verlauf für andauernde Belastungen mit vierter Zeile für Bioakkumulation
#' plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "andauernd", optin_mischtox_S = TRUE)
#'
#' # Ausführlicher Verlauf für kurzzeitige Belastungen
#' plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "kurzzeitig")
#'
#' # Zusammengefasster Verlauf für andauernde Belastungen
#' plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr,
#'   modus = "andauernd",
#'   plot_zusammenfassung = "mischproben"
#' )
#'
#' # Zusammengefasster Verlauf für kurzzeitige Belastungen
#' plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr,
#'   modus = "kurzzeitig",
#'   plot_zusammenfassung = "mischproben"
#' )
#'
#' # Zusammengefasster Verlauf für Stichproben
#' plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr,
#'   modus = "kurzzeitig",
#'   plot_zusammenfassung = "stichproben"
#' )
plot_misch_mixtox_verlauf <- function(rq_ue_daten,
                                      stationscode = NULL,
                                      jahr = NULL,
                                      modus = c("andauernd", "kurzzeitig"),
                                      plot_zusammenfassung = "keine",
                                      optin_mischtox_S = FALSE) {
  modus <- match.arg(modus)
  # Stichproben nur für Zusammenfassungsplot relevant
  if (plot_zusammenfassung == "stichproben") {
    rq_ue_daten <- dplyr::filter(rq_ue_daten, is.na(.data$ENDEPROBENAHME))
    probenart <- "Stichproben"
    modus <- "kurzzeitig"
  } else if (plot_zusammenfassung == "mischproben") {
    rq_ue_daten <- dplyr::filter(rq_ue_daten, !is.na(.data$ENDEPROBENAHME))
    probenart <- "Mischproben"
  } else if (plot_zusammenfassung == "keine") {
    rq_ue_daten <- dplyr::filter(rq_ue_daten, !is.na(.data$ENDEPROBENAHME))
  } else {
    cli::cli_abort(c(
      "x" = "Ung\u00fcltiges Argument f\u00fcr plot_zusammenfassung",
      "i" = "M\u00f6gliche Werte: 'stichproben', 'mischproben', 'keine'"
    ))
  }

  if (is.null(jahr)) {
    jahr <- unique(lubridate::year(rq_ue_daten$BEGINNPROBENAHME))
  }

  if (is.null(stationscode)) {
    stationscode <- unique(rq_ue_daten$CODE)
  }

  rq_data <- rq_ue_daten |>
    dplyr::filter(.data$CODE %in% .env$stationscode, .data$Jahr %in% .env$jahr)

  modus_cfg <- get_modus_config(modus)
  switchKriterium <- modus_cfg$switchKriterium

  mixtox_data <- berechne_mixtox(rq_data) |>
    dplyr::filter(.data$Kriterium %in% .env$switchKriterium)

  if (modus == "andauernd") {
    mixtox_data <- dplyr::filter(mixtox_data, .data$Tage >= 10)
  }

  # Falls nicht explizit gewünscht wird, dass wir secondary toxicity auch bewerten, werden diese Resultate ausgeblendet, damit keine Änderung gegenüber v1.1.0 entsteht
  if (!optin_mischtox_S) {
    mixtox_data <- dplyr::filter(mixtox_data, .data$Ziel != "Bioakkumulation")
  }

  farbskala_tox <- farbskala_bewertung_ecotox()

  if (plot_zusammenfassung == "keine") {
    mixtox_pobj <- ggplot2::ggplot(mixtox_data) +
      ggplot2::facet_grid(.data$STANDORT ~ ., scales = "free_y") +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = .data$BEGINNPROBENAHME,
          xmax = .data$ENDEPROBENAHME,
          ymin = .data$Ziel_num - 0.5,
          ymax = .data$Ziel_num + 0.5,
          fill = .data$Beurteilung
        ),
        colour = "white",
        linewidth = 0.1
      ) +
      ggplot2::scale_x_datetime(
        "",
        date_breaks = "2 months",
        labels = lagged_labels_jahr,
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(
        breaks = mixtox_data$Ziel_num,
        labels = mixtox_data$Ziel,
        expand = c(0, 0)
      ) +
      # drop = FALSE, damit alle Levels der MSK-Skala in der Legende geplottet werden
      ggplot2::scale_fill_manual(values = farbskala_tox, drop = FALSE) +
      ggplot2::guides(fill = "none") +
      ggplot2::ggtitle(sprintf("Beurteilung Mischungstoxizit\u00e4t (%s)", modus)) +
      plot_theme_proto() +
      ggplot2::theme(
        axis.ticks.length = ggplot2::unit(.25, "cm"),
        axis.text = ggplot2::element_text(colour = "black")
      )
  }

  if (plot_zusammenfassung != "keine") {
    mixtox_summary <- mixtox_data |>
      dplyr::group_by(.data$CODE, .data$Jahr, .data$Ziel, .data$Ziel_num) |>
      dplyr::slice_max(order_by = .data$RQ, n = 1, with_ties = FALSE, na_rm = FALSE) |>
      dplyr::ungroup()

    mixtox_pobj <- ggplot2::ggplot(mixtox_summary) +
      ggplot2::facet_grid(.data$CODE ~ ., scales = "free_y") +
      ggplot2::geom_rect(
        ggplot2::aes(
          xmin = .data$Jahr - 0.5,
          xmax = .data$Jahr + 0.5,
          ymin = .data$Ziel_num - 0.5,
          ymax = .data$Ziel_num + 0.5,
          fill = .data$Beurteilung
        ),
        colour = "white",
        linewidth = 0.1
      ) +
      ggplot2::scale_x_continuous("",
        breaks = unique(mixtox_summary$Jahr),
        expand = c(0, 0)
      ) +
      # Wir zeigen hier zusätzlich die gerundeten Werte
      ggplot2::geom_text(ggplot2::aes(x = .data$Jahr, y = .data$Ziel_num, label = signif(.data$RQ, 2)), size = 4) +
      ggplot2::scale_y_continuous("",
        breaks = mixtox_data$Ziel_num,
        labels = mixtox_data$Ziel,
        expand = c(0, 0)
      ) +
      ggplot2::scale_fill_manual(values = farbskala_tox, drop = FALSE) +
      ggplot2::guides(fill = "none") +
      ggplot2::ggtitle(sprintf("Beurteilung Mischungstoxizit\u00e4t (%s)", probenart)) +
      plot_theme_proto() +
      ggplot2::theme(
        axis.ticks.length = ggplot2::unit(.25, "cm"),
        axis.text = ggplot2::element_text(colour = "black")
      )
  }

  mixtox_pobj
}

#' Zeitreihe Häufigkeitsverteilung Mischungstoxizität
#'
#' Zeigt die Häufigkeitsverteilung der Beurteilung der Mischungstoxizität über die Zeit für eine Station.
#'
#' @inheritParams plot_misch_mixtox_verlauf
#'
#' @return ggplot2 Plot-Objekt
#' @export
#'
#' @examples
#' # Häufigkeitsverteilung für andauernde Belastungen
#' plot_misch_mixtox_haeufigkeit(rq_ue_beispiel_mvwizr,
#'   stationscode = "URT010",
#'   modus = "andauernd"
#' )
#'
#' # Häufigkeitsverteilung für kurzzeitige Belastungen
#' plot_misch_mixtox_haeufigkeit(rq_ue_beispiel_mvwizr,
#'   stationscode = "URT010",
#'   modus = "kurzzeitig"
#' )
plot_misch_mixtox_haeufigkeit <- function(rq_ue_daten,
                                          stationscode,
                                          modus = c("andauernd", "kurzzeitig"),
                                          optin_mischtox_S = FALSE) {
  modus <- match.arg(modus)
  rq_data <- rq_ue_daten |>
    dplyr::filter(.data$CODE %in% .env$stationscode, !is.na(.data$ENDEPROBENAHME))

  if (nrow(rq_data) == 0) {
    cli::cli_abort("Keine Daten f\u00fcr Station {stationscode} vorhanden.", class = "mvwizr_error_empty_dataset")
  }

  modus_cfg <- get_modus_config(modus)
  switchKriterium <- modus_cfg$switchKriterium

  mixtox_data <- berechne_mixtox(rq_data) |>
    dplyr::filter(.data$Kriterium %in% .env$switchKriterium)

  if (modus == "andauernd") {
    mixtox_data <- dplyr::filter(mixtox_data, .data$Tage >= 10)
  }

  # Falls nicht explizit gewünscht wird, dass wir secondary toxicity auch bewerten, werden diese Resultate ausgeblendet, damit keine Änderung gegenüber v1.1.0 entsteht
  if (!optin_mischtox_S) {
    mixtox_data <- dplyr::filter(mixtox_data, .data$Ziel != "Bioakkumulation")
  }

  farbskala_tox <- farbskala_bewertung_ecotox()

  stationsname <- unique(mixtox_data$STANDORT)

  pobj <- ggplot2::ggplot(mixtox_data, ggplot2::aes(x = .data$Jahr, fill = .data$Beurteilung)) +
    # geom_bar berechnet automatisch die Anteile der Beurteilungen. Mit position = fill werden relative Anteile auf 100% geplottet
    ggplot2::geom_bar(position = "fill", show.legend = TRUE) +
    ggplot2::facet_wrap(~Ziel, scales = "free_y") +
    ggplot2::scale_x_continuous(
      "",
      breaks = unique(mixtox_data$Jahr),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_continuous("H\u00e4ufigkeit (Prozent)",
      breaks = seq(0, 1, by = 0.1),
      # Darstellung als Prozent (100) anstelle Anteil (1)
      labels = scales::percent(seq(0, 1, by = 0.1))
    ) +
    ggplot2::scale_fill_manual(values = farbskala_tox, drop = FALSE) +
    ggplot2::ggtitle(sprintf("Verteilung Mischungstoxizit\u00e4t (Station: %s)", stationsname)) +
    plot_theme_proto() +
    ggplot2::theme(
      axis.ticks.length = ggplot2::unit(.25, "cm"),
      axis.text = ggplot2::element_text(colour = "black")
    )

  pobj
}

