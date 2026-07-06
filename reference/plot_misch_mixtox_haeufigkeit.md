# Zeitreihe Häufigkeitsverteilung Mischungstoxizität

Zeigt die Verteilung der Beurteilung der Mischungstoxizität über die
Zeit für eine Station. Auf der y-Achse wird die Anzahl bewerteter Wochen
pro Jahr gezeigt (aufsummierte Probendauer je Beurteilung). Bei
durchgehender Bewertung erreichen die Balken das Jahresmaximum (~52
Wochen); nicht bewertete Zeiträume erscheinen als kürzere Balken.

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
    andauernden Grenzwert in der GSchV. Verwendet werden
    Zweiwochenmischkonzentrationen (Proben ab 13 Tagen direkt; Proben
    von 10 bis unter 13 Tagen werden zeitproportional über 14 Tage
    hochgerechnet, wobei die unbeprobte Zeit als Konzentration 0
    angenommen wird). Die Beurteilung erfolgt anhand des CQK.

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
