# Aufsummierte andauernde GSchV-Überschreitungen plotten

Zeigt die andauernden Überschreitungen pro Jahr und Station. Dafür
werden nur Proben mit einer Messdauer \>= 10 Tage berücksichtigt.

## Verwendung

``` r
plot_misch_ue_summe(rq_ue_daten, stationscode = NULL, jahr = NULL)
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
plot_misch_ue_summe(rq_ue_beispiel_mvwizr)

```
