# MV-Daten des GBL (Kt. Bern) einlesen

Liest MV-Daten des GBL (Kt. Bern) ein und bereitet sie für die weitere
Verarbeitung auf. Achtung: Falls `bSaP == TRUE`, werden
3.5-Tage-Mischproben, die im Intervall (Beginn- bis Enddatum ohne Zeit)
der berechneten 14-Tage-Mischproben liegen, entfernt! Für die Auswertung
akuter Überschreitungen muss deshalb `bSaP == FALSE` gesetzt werden.

## Verwendung

``` r
einlesen_mv_gbl(
  mv_daten_pfad,
  vsa_lookup_pfad = NULL,
  bafu_lookup_pfad = NULL,
  bSaP = FALSE
)
```

## Argumente

- mv_daten_pfad:

  Pfad zur kommaseparierten Datei mit den MV-Daten

- vsa_lookup_pfad:

  Pfad zur VSA-Tabelle `Tab_Substanzen.xlsx`

- bafu_lookup_pfad:

  Pfad zum BAFU-Namen-Lookup

- bSaP:

  Logisch (Vorgabe `FALSE`). Sollen berechnete Mischproben verwendet
  werden?

## Rückgabewert

Dataframe mit MV-Daten

## Beispiele

``` r
mv_daten_pfad <- system.file("extdata", "Daten_MV_GBL_2019_2020.txt", package = "mvwizr")

# Ab mvwizr v1.2 können vsa_lookup_pfad und bafu_lookup_pfad leer gelassen werden
# - es wird dann die mit dem Paket gebundelte Datei verwendet.
mvdaten <- einlesen_mv_gbl(mv_daten_pfad)
#> Warning: ! VSA-Lookup: 2 mehrfache Bezeichnungen (VSA Parameter-ID) pro Substanz_ID
#>   gefunden. Verwende tiefere Substanz_ID.
#> ℹ Betroffen: 8_2-FTCA, SiO2
#> Warning: ! Nicht für alle Substanzen Bestimmungsgrenzen gefunden
#> ℹ Es wurden maximale Bestimmungsgrenzen für 155 von 159 Substanzen gefunden.
```
