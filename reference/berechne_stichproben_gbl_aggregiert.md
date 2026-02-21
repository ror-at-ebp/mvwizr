# Stichprobenauswertung GBL

Wertet Stichproben nach Parametergruppe aus. Dazu wird pro Station und
Jahr das Perzentil (Vorgabe: 90) der Werte pro Substanz berechnet.
Danach wird die Summe der Perzentile pro Parametergruppe berechnet und
zurückgegeben.

## Verwendung

``` r
berechne_stichproben_gbl_aggregiert(mv_daten, perzentil = 90)
```

## Argumente

- mv_daten:

  Tibble mit den MV-Daten aus der Funktion `einlesen_mv_gbl` resp gemäss
  Spezifikationen.

- perzentil:

  Perzentil, das berechnet werden soll pro Substanz (Vorgabe: 90).

## Rückgabewert

Tibble mit aggregierten Stichproben
