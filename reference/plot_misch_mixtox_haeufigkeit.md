# Zeitreihe Häufigkeitsverteilung Mischungstoxizität

Zeigt die Häufigkeitsverteilung der Beurteilung der Mischungstoxizität
über die Zeit für eine Station.

## Verwendung

``` r
plot_misch_mixtox_haeufigkeit(
  rq_ue_daten,
  stationscode,
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
# Häufigkeitsverteilung für andauernde Belastungen
plot_misch_mixtox_haeufigkeit(rq_ue_beispiel_mvwizr,
  stationscode = "URT010",
  modus = "andauernd"
)


# Häufigkeitsverteilung für kurzzeitige Belastungen
plot_misch_mixtox_haeufigkeit(rq_ue_beispiel_mvwizr,
  stationscode = "URT010",
  modus = "kurzzeitig"
)
```
