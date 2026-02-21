# Entferne SaP, welche für bSaP verwendet wurden

Bei den (gemessenen) Mischproben unterscheidet das NAWA-MV-Protokoll
zwischen 3.5-Tage-Proben und 14-Tage-Proben (SaP). Ausserdem können
berechnete 14-Tage-Proben (bSaP) aus 4 kürzeren Mischproben errechnet
werden. Damit diese Werte nicht doppelt gezählt werden, sollten solche
kürzeren Proben, die für die Berechnung verwendet wurden, aus den Daten
entfernt werden.

## Verwendung

``` r
prozessiere_bSaP(mv_daten, bSaP_identifier)
```

## Argumente

- mv_daten:

  Tibble mit den MV-Daten aus der Funktion `einlesen_mv_gbl` oder
  `einlesen_nawa` resp. `batch_einlesen_nawa`.

- bSaP_identifier:

  Name der Spalte als String mit der Information, ob es sich um
  berechnete Mischproben ("bSaP") oder gemessene Mischproben ("SaP")
  handelt. Erlaubte Werte in Spalte: "bSaP", "SaP" und "S" (für
  Stichproben - werden nicht bearbeitet).

## Rückgabewert

mv-daten mit entfernten Datensätzen

## Details

Für GBL-Daten wird diese Funktion automatisch aufgerufen. Da beim
NAWA-MV-Format zur Zeit keine Kennzeichnung von bSaP-Proben vorgesehen
ist, muss dieser Schritt nach dem Einlesen erfolgen, und bSaP Proben
müssen manuell gekennzeichnet werden.

Achtung: Diese Berechnungen funktionieren nur, wenn die 3.5-Tage Proben
zeitlich innerhalb der 14-Tage Proben liegen oder sich mit dem Zeitraum
(auf Tag gerundet) decken. Falls die zur Berechnung verwendeten
3.5-Tage-Mischproben das Intervall der betreffenden bSaP 14-Tage-Probe
überschneiden, so werden nur die 3.5-Tage-Proben, die innerhalb des
Zeitraums liegen, entfernt!

## Beispiele

``` r
# Bei NAWA-MV-Dateien muss der bSaP-Identifier manuell hinzugefügt werden.
# Im folgenden Beispiel werden zwei Dateien eingelesen:
# - NAWA_ohne_bSaP_Bsp.xlsx enthält gemessene Mischproben (3.5d und 14d)
# - NAWA_mit_bSaP_Bsp.csv enthält die berechneten Mischproben

mv_bsap_pfade <- system.file("extdata", c(
  "NAWA_ohne_bSaP_Bsp.xlsx",
  "NAWA_mit_bSaP_Bsp.csv"
), package = "mvwizr")
out <- batch_einlesen_nawa(mv_bsap_pfade) |>
  dplyr::mutate(PROBEARTID = dplyr::if_else(.data$filename ==
    "NAWA_mit_bSaP_Bsp.csv", "bSaP", .data$PROBEARTID))
#> ℹ Keine Import-Manifest-Datei angegeben. Lese Dateien ein und errate Funktionsparameter...
#> 
#> ── (1/2)  Lese NAWA_ohne_bSaP_Bsp.xlsx ... ─────────────────────────────────────
#> Warning: ! VSA-Lookup: 2 mehrfache Bezeichnungen (VSA Parameter-ID) pro Substanz_ID
#>   gefunden. Verwende tiefere Substanz_ID.
#> ℹ Betroffen: 8_2-FTCA, SiO2
#> ℹ Lese MV-Daten von Excel-Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_ohne_bSaP_Bsp.xlsx ein.
#> ℹ Versuche Start des Tabellen-Headers der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_ohne_bSaP_Bsp.xlsx zu erraten.
#> ✔ Erkannter Header-Start: Zeile 8.
#> ℹ Versuche Sprache der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_ohne_bSaP_Bsp.xlsx zu erraten.
#> ✔ Erkannte Sprache: DE.
#> ℹ Versuche Parameter-Feld der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_ohne_bSaP_Bsp.xlsx zu erraten.
#> ✔ Erkannter Parameter: BAFU_Parameter_ID.
#> ! Bestimmungsgrenzen wurden nicht als Zahlen eingelesen - versuche Typenkonvertierung.
#> ℹ Normalisiere Einheiten der MV-Daten auf µg/l.
#> ℹ Max./min. Bestimmungsgrenzen der MV-Daten bestimmen...
#> 
#> ── (2/2)  Lese NAWA_mit_bSaP_Bsp.csv ... ───────────────────────────────────────
#> Warning: ! VSA-Lookup: 2 mehrfache Bezeichnungen (VSA Parameter-ID) pro Substanz_ID
#>   gefunden. Verwende tiefere Substanz_ID.
#> ℹ Betroffen: 8_2-FTCA, SiO2
#> ℹ Lese MV-Daten von Text-Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_mit_bSaP_Bsp.csv ein.
#> ℹ Versuche Encoding der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_mit_bSaP_Bsp.csv zu erraten.
#> ✔ Erkanntes Encoding: UTF-8
#> ℹ Versuche Trennzeichen der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_mit_bSaP_Bsp.csv zu erraten.
#> ✔ Erkanntes Trennzeichen: ;
#> ℹ Versuche Start des Tabellen-Headers der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_mit_bSaP_Bsp.csv zu erraten.
#> ✔ Erkannter Header-Start: Zeile 1.
#> ℹ Versuche Sprache der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_mit_bSaP_Bsp.csv zu erraten.
#> ✔ Erkannte Sprache: DE.
#> ℹ Lese NAWA-MV-Daten von /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_mit_bSaP_Bsp.csv ein.
#> ℹ Versuche Parameter-Feld der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_mit_bSaP_Bsp.csv zu erraten.
#> ✔ Erkannter Parameter: BAFU_Parameter_ID.
#> ℹ Normalisiere Einheiten der MV-Daten auf µg/l.
#> ℹ Max./min. Bestimmungsgrenzen der MV-Daten bestimmen...
#> ℹ Kombiniere MV-Daten aus 2 Dateien...
#> ✔ MV-Daten erfolgreich eingelesen und kombiniert.

mv_bsap <- prozessiere_bSaP(out, bSaP_identifier = "PROBEARTID") |>
  dplyr::mutate(Dauer = difftime(.data$ENDEPROBENAHME,
    .data$BEGINNPROBENAHME,
    units = "days"
  ))
```
