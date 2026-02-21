# Zusammenfassung von Mischungstoxizitäten für Stichproben plotten

Hinweis: Hierbei handelt es sich nur um einen Wrapper um
`plot_misch_mixtox_verlauf` für dieselben Plots aber mit Stichproben.

## Verwendung

``` r
plot_stich_mixtox_zf(rq_ue_daten, stationscode = NULL, jahr = NULL)
```

## Argumente

- rq_ue_daten:

  Dataframe mit Output der Funktion
  [`berechne_rq_ue()`](https://ror-at-ebp.github.io/mvwizr/reference/berechne_rq_ue.md)

- stationscode:

  Station, für welche der Plot erstellt werden soll.

- jahr:

  Jahr, für welches der Überschreitungsplot erstellt werden soll.

## Rückgabewert

ggplot2 Plot-Objekt

## Beispiele

``` r
plot_stich_mixtox_zf(rq_ue_beispiel_mvwizr)
#> Warning: Removed 10 rows containing missing values or values outside the scale range
#> (`geom_text()`).
```
