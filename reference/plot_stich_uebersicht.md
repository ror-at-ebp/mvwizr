# Stationsübersicht Stichproben

Diese Übersichtsfunktion plottet sämtliche Substanzen in allen
Stichproben in einem als Raster über die Zeit und benutzt eine
logarithmische Farbskala um relevante Substanzen und Veränderungen
hervorzuheben.

## Verwendung

``` r
plot_stich_uebersicht(mv_daten, stationscode, jahr = NULL)
```

## Argumente

- mv_daten:

  Dataframe mit aufbereiteten MV-Daten gemäss Spezifikation

- stationscode:

  Station, für welche der Plot erstellt werden soll

- jahr:

  Jahre (numerischer Vektor), für welche ein Verlauf geplottet werden
  soll. Falls `NULL` (Vorgabewert), werden alle verfügbaren Daten
  geplottet.

## Rückgabewert

ggplot2 Plot-Objekt

## Beispiele

``` r
# Stichprobenübersichten für zwei Stationen
plot_stich_uebersicht(mvdaten_beispiel_mvwizr, stationscode = "SA51")

plot_stich_uebersicht(mvdaten_beispiel_mvwizr, stationscode = "KI52")
```
