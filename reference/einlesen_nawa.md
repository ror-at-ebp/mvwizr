# NAWA-MV-Daten einlesen

Liest MV-Daten im BAFU-NAWA-Format ein. Falls die Datei einen Header
aufweist vor dem Start der eigentlichen Tabelle, so wird dieser
übersprungen. Um mehrere Dateien im NAWA-Format auf einmal einzulesen,
kann die Funktion
[`batch_einlesen_nawa()`](https://ror-at-ebp.github.io/mvwizr/reference/batch_einlesen_nawa.md)
verwendet werden.

## Verwendung

``` r
einlesen_nawa(
  nawa_mv,
  vsa_lookup_pfad = NULL,
  bafu_lookup_pfad = NULL,
  delimiter = NA_character_,
  encoding = NA_character_,
  lang = NA_character_,
  parameter = NA_character_,
  header = NA_integer_
)
```

## Argumente

- nawa_mv:

  Entweder Pfad zur Datei im NAWA-Format (Excel oder Text) oder ein
  Dataframe im NAWA-Format (z.B. WISKI-Export)

- vsa_lookup_pfad:

  Pfad zur VSA-Tabelle `Tab_Substanzen.xlsx`

- bafu_lookup_pfad:

  Pfad zum BAFU-Namen-Lookup

- delimiter:

  Trennzeichen für Textdateien. Falls `NULL`, wird versucht, das
  Trennzeichen zu erraten. Standard ist `NULL`.

- encoding:

  Encoding der Textdatei. Falls `NULL`, wird versucht, das Encoding zu
  erraten. Standard ist `NULL`.

- lang:

  Sprache der Datei. Falls `NULL`, wird versucht, die Sprache zu
  erraten. Mögliche Werte sind "DE" (Deutsch) oder "FR" (Französisch).
  Standard ist `NULL`.

- parameter:

  Angabe, was im "Parameter"-Feld der MV-Datei steht (da nicht
  konsistent verwendet). Mögliche Werte sind "BAFU_Parameter_ID" (für
  den BAFU-Schlüssel ohne Sonderzeichen), "BAFU_Bez_DE" (Deutsche
  Bezeichnung) oder "BAFU_Bez_FR" (Französische Bezeichnung). Falls
  `NULL`, wird versucht, das Parameter-Feld zu erraten. Standard ist
  `NULL`.

- header:

  Zeilennummer, ab der die eigentliche Tabelle beginnt. Falls `NULL`,
  wird versucht, die Header-Zeile zu erraten. Standard ist `NULL`.

## Rückgabewert

Dataframe mit MV-Daten

## Details

Die Funktion liest entweder eine (einzelne) Datei im Excel
(.xlsx)-Format ein oder als Text mit Trennzeichen (.csv, .txt). Die
Funktion kann anstelle einer Datei auch ein Dataframe im NAWA-Format
entgegennehmen (z.B. über Datenbank-Verbindung abgerufen).

Duplikate (z.B. auch für Messungen auf mehreren Geräten) werden
entfernt, indem jeweils der höchste Messwert für die gleiche Probe
(Station, Beginn- und Enddatum) und Substanz verwendet wird.

Bestimmungsgrenzen werden aus den eingelesenen Daten bestimmt, wobei die
maximale und minimale Bestimmungsgrenze (über die gesamte Messperiode in
den Daten) für jede Substanz ermittelt wird - die min. und max.
Bestimmungsgrenzen sind also konstant im Datensatz. Falls eine Substanz
keine Bestimmungsgrenze hat, so wird sie nicht entfernt, sondern es wird
eine Warnung ausgegeben.

Alle Einheiten werden (für die weiteren Berechnungen) automatisch auf
µg/l normalisiert.

Messungen von Substanzen, die nicht einer BAFU-Parameter-ID und einer
VSA Substanz-ID zugeordnet werden können, werden entfernt.

## Beispiele

``` r
# Testen der Einlesefunktion mit Heuristik
nawa_mv_fr <- system.file("extdata", "NAWA_Bsp_4.xlsx", package = "mvwizr")

out <- einlesen_nawa(nawa_mv_fr)
#> Warning: ! VSA-Lookup: 2 mehrfache Bezeichnungen (VSA Parameter-ID) pro Substanz_ID
#>   gefunden. Verwende tiefere Substanz_ID.
#> ℹ Betroffen: 8_2-FTCA, SiO2
#> ℹ Lese MV-Daten von Excel-Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_4.xlsx ein.
#> ℹ Versuche Start des Tabellen-Headers der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_4.xlsx zu erraten.
#> ✔ Erkannter Header-Start: Zeile 8.
#> ℹ Versuche Sprache der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_4.xlsx zu erraten.
#> ✔ Erkannte Sprache: FR.
#> ℹ Versuche Parameter-Feld der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_4.xlsx zu erraten.
#> ✔ Erkannter Parameter: BAFU_Bez_FR.
#> ! Felder BEGINNPROBENAHME oder ENDEPROBENAHME wurden nicht als Datum eingelesen - versuche Typenkonvertierung.
#> ! Bestimmungsgrenzen wurden nicht als Zahlen eingelesen - versuche Typenkonvertierung.
#> ℹ Normalisiere Einheiten der MV-Daten auf µg/l.
#> ℹ Max./min. Bestimmungsgrenzen der MV-Daten bestimmen...
#> Warning: ! /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_4.xlsx: Nicht alle
#>   Substanzen, die in den MV-Daten gefunden wurden, konnten einer BAFU
#>   Parameter-ID zugeordnet werden.
#> ℹ Es wurden 3 Substanzen ohne BAFU Parameter-ID gefunden: Chlorpyrifos;
#>   Chlorpyrifos-méthyl; Dichlobenil
#> Warning: ! /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_4.xlsx: Nicht alle
#>   Substanzen, die in den MV-Daten gefunden wurden, konnten einer VSA
#>   ID_Substanz zugeordnet werden. Daten ohne ID_Substanz oder PARAMETERID_BAFU
#>   werden entfernt.
#> ℹ 2 Fehlende Zuordnungen für PO4-P-filtriert; P-tot

# Testen der Einlesefunktion mit Angabe von Argumenten
out2 <- einlesen_nawa(nawa_mv_fr,
  lang = "FR", parameter = "BAFU_Bez_FR",
  header = 8L
)
#> Warning: ! VSA-Lookup: 2 mehrfache Bezeichnungen (VSA Parameter-ID) pro Substanz_ID
#>   gefunden. Verwende tiefere Substanz_ID.
#> ℹ Betroffen: 8_2-FTCA, SiO2
#> ℹ Lese MV-Daten von Excel-Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_4.xlsx ein.
#> ! Felder BEGINNPROBENAHME oder ENDEPROBENAHME wurden nicht als Datum eingelesen - versuche Typenkonvertierung.
#> ! Bestimmungsgrenzen wurden nicht als Zahlen eingelesen - versuche Typenkonvertierung.
#> ℹ Normalisiere Einheiten der MV-Daten auf µg/l.
#> ℹ Max./min. Bestimmungsgrenzen der MV-Daten bestimmen...
#> Warning: ! /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_4.xlsx: Nicht alle
#>   Substanzen, die in den MV-Daten gefunden wurden, konnten einer BAFU
#>   Parameter-ID zugeordnet werden.
#> ℹ Es wurden 3 Substanzen ohne BAFU Parameter-ID gefunden: Chlorpyrifos;
#>   Chlorpyrifos-méthyl; Dichlobenil
#> Warning: ! /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_4.xlsx: Nicht alle
#>   Substanzen, die in den MV-Daten gefunden wurden, konnten einer VSA
#>   ID_Substanz zugeordnet werden. Daten ohne ID_Substanz oder PARAMETERID_BAFU
#>   werden entfernt.
#> ℹ 2 Fehlende Zuordnungen für PO4-P-filtriert; P-tot

# Vergleich der Resultate
identical(out, out2)
#> [1] TRUE
```
