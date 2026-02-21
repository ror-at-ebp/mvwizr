# Stationsübersicht Ökotoxbeurteilung plotten

Plottet die Ökotoxbeurteilung für Einzelstoffe (kurzzeitig oder
andauernd) und Mischtoxizitäten pro Jahr und Station zusammen.

## Verwendung

``` r
plot_misch_oekotox_uebersicht(
  rq_ue_daten,
  stationscode,
  jahr,
  modus = c("andauernd", "kurzzeitig"),
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
plot_misch_oekotox_uebersicht(rq_ue_beispiel_mvwizr, "URT010", 2020)


plot_misch_oekotox_uebersicht(rq_ue_beispiel_mvwizr, "MUS001", 2020, modus = "kurzzeitig")


plot_misch_oekotox_uebersicht(rq_ue_beispiel_mvwizr, "MUS001", 2020)
```
