# BAFU-Namens-Lookup einlesen

BAFU-Namens-Lookup einlesen

## Verwendung

``` r
einlesen_bafu_lookup(bafu_lookup_pfad, alle_felder = FALSE)
```

## Argumente

- bafu_lookup_pfad:

  Pfad zur Exceldatei mit dem Namens-Lookup

- alle_felder:

  Logisch (Vorgabe: `FALSE`). Sollen alle Spalten der Tabelle exportiert
  werden?

## Rückgabewert

Dataframe mit Zuordnung der französischen und deutschen Bezeichnungen
zur eindeutigen BAFU Parameter-ID
