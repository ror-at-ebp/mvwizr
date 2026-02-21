# Verlauf von Mischungstoxizitäten plotten

Zeigt den Verlauf der Mischungstoxizitäten pro Jahr (fortlaufend
möglich) und Stationen.

## Verwendung

``` r
plot_misch_mixtox_verlauf(
  rq_ue_daten,
  stationscode = NULL,
  jahr = NULL,
  modus = c("andauernd", "kurzzeitig"),
  plot_zusammenfassung = "keine",
  optin_mischtox_S = FALSE
)
```

## Argumente

- rq_ue_daten:

  Dataframe mit Output der Funktion
  [`berechne_rq_ue()`](https://ror-at-ebp.github.io/mvwizr/reference/berechne_rq_ue.md)

- stationscode:

  Station, für welche der Plot erstellt werden soll.

- jahr:

  Jahr, für welches der Überschreitungsplot erstellt werden soll.

- modus:

  - `"andauernd"`: Berücksichtigt nur Stoffe mit einem spezifischen
    andauernden Grenzwert in der GSchV. Nur Proben mit einer Dauer von
    \>= 10 Tagen werden verwendet und unter Verwendung des CQK
    beurteilt.

  - `"kurzzeitig"`: Berücksichtigt nur Stoffe mit einem spezifischen
    akuten Grenzwert in der GSchV. Berücksichtigt alle Proben (ausser
    Stichproben) und verwendet das AQK zur Beurteilung.

- plot_zusammenfassung:

  Entweder `NULL` (Vorgabe), "stichproben" oder "mischproben". Falls
  einer der letzeren beiden, wird pro Jahr und nicht pro Monat
  aggregiert (entweder nur für Stichproben oder nur für Mischproben).
  Für Stichproben muss der `modus` auf "kurzzeitig" gesetzt werden.

- optin_mischtox_S:

  Logisch (Vorgabe: `FALSE`). Ab v1.2 unterstützt mvwizr auch die
  Anzeige der Bioakkumulation / Secondary toxicity (`S_chron`). Falls
  `TRUE`, wird eine vierte Zeile bei den Mischtoxizitäten angezeigt,
  falls sie in den Daten vorhanden ist. Falls `FALSE`, wird sie nicht
  angezeigt.

## Rückgabewert

ggplot2 Plot-Objekt

## Beispiele

``` r
# Ausführlicher Verlauf für andauernde Belastungen
plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "andauernd")


# Ausführlicher Verlauf für andauernde Belastungen mit vierter Zeile für Bioakkumulation
plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "andauernd", optin_mischtox_S = TRUE)


# Ausführlicher Verlauf für kurzzeitige Belastungen
plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr, modus = "kurzzeitig")


# Zusammengefasster Verlauf für andauernde Belastungen
plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr,
  modus = "andauernd",
  plot_zusammenfassung = "mischproben"
)


# Zusammengefasster Verlauf für kurzzeitige Belastungen
plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr,
  modus = "kurzzeitig",
  plot_zusammenfassung = "mischproben"
)


# Zusammengefasster Verlauf für Stichproben
plot_misch_mixtox_verlauf(rq_ue_beispiel_mvwizr,
  modus = "kurzzeitig",
  plot_zusammenfassung = "stichproben"
)
#> Warning: Removed 10 rows containing missing values or values outside the scale range
#> (`geom_text()`).
```
