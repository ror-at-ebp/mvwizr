# Batch-Einlesen von NAWA-MV-Daten

Liest mehrere NAWA-MV-Daten-Dateien ein, entweder über ein
Import-Manifest oder eine Liste von Dateipfaden. Die Funktion kann auch
die Parameter wie Encoding, Header, Delimiter und Sprache erraten, wenn
diese nicht angegeben sind (oder nur teilweise).

## Verwendung

``` r
batch_einlesen_nawa(
  nawa_mv_pfade = NULL,
  vsa_lookup_pfad = NULL,
  bafu_lookup_pfad = NULL,
  import_manifest = NULL
)
```

## Argumente

- nawa_mv_pfade:

  Dateipfade zu MV-Daten im NAWA-Format (Excel oder Text). Falls `NULL`,
  muss das Import-Manifest verwendet werden.

- vsa_lookup_pfad:

  Pfad zur VSA-Tabelle `Tab_Substanzen.xlsx`

- bafu_lookup_pfad:

  Pfad zum BAFU-Namen-Lookup

- import_manifest:

  Pfad zu einem Import-Manifest, das die MV-Daten-Dateien und deren
  Parameter enthält. Falls `NULL`, müssen die Dateipfade bei
  `nawa_mv_pfade` angegeben werden und alle Parameter werden geraten.
  Das Manifest muss eine Excel-Datei sein, die die Spalten `file`,
  `encoding`, `header`, `delimiter` und `lang` enthält. Mittels
  [`schreibe_nawa_import_manifest_template()`](https://ror-at-ebp.github.io/mvwizr/reference/schreibe_nawa_import_manifest_template.md)
  kann ein leeres Manifest erstellt werden, das dann mit den
  entsprechenden Dateien gefüllt werden kann.

## Rückgabewert

Dataframe mit kombinierten MV-Daten aus allen angegebenen Dateien
(Achtung: Entfernt keine Duplikate zwischen den Dateien, sondern nur
innerhalb einer Datei).

## Beispiele

``` r
# Batch-Einlesen mit Heuristik
# Die Zahlreichen Meldungen der Funktion sind hilfreich beim Auffinden von Problemen

nawa_mv_pfade <- system.file("extdata", c(
  "NAWA_Bsp_1.xlsx",
  "NAWA_Bsp_2.xlsx", "NAWA_Bsp_3.csv", "NAWA_Bsp_4.xlsx",
  "NAWA_Bsp_5.csv"
), package = "mvwizr")

out <- batch_einlesen_nawa(nawa_mv_pfade)
#> ℹ Keine Import-Manifest-Datei angegeben. Lese Dateien ein und errate Funktionsparameter...
#> 
#> ── (1/5)  Lese NAWA_Bsp_1.xlsx ... ─────────────────────────────────────────────
#> Warning: ! VSA-Lookup: 2 mehrfache Bezeichnungen (VSA Parameter-ID) pro Substanz_ID
#>   gefunden. Verwende tiefere Substanz_ID.
#> ℹ Betroffen: 8_2-FTCA, SiO2
#> ℹ Lese MV-Daten von Excel-Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_1.xlsx ein.
#> ℹ Versuche Start des Tabellen-Headers der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_1.xlsx zu erraten.
#> ✔ Erkannter Header-Start: Zeile 8.
#> ℹ Versuche Sprache der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_1.xlsx zu erraten.
#> ✔ Erkannte Sprache: DE.
#> ℹ Versuche Parameter-Feld der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_1.xlsx zu erraten.
#> ✔ Erkannter Parameter: BAFU_Bez_DE.
#> ! Felder BEGINNPROBENAHME oder ENDEPROBENAHME wurden nicht als Datum eingelesen - versuche Typenkonvertierung.
#> ! Bestimmungsgrenzen wurden nicht als Zahlen eingelesen - versuche Typenkonvertierung.
#> ℹ Normalisiere Einheiten der MV-Daten auf µg/l.
#> ℹ Max./min. Bestimmungsgrenzen der MV-Daten bestimmen...
#> Warning: ! /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_1.xlsx: Nicht alle
#>   Substanzen, die in den MV-Daten gefunden wurden, konnten einer VSA
#>   ID_Substanz zugeordnet werden. Daten ohne ID_Substanz oder PARAMETERID_BAFU
#>   werden entfernt.
#> ℹ 6 Fehlende Zuordnungen für Al-geloest; Br-; el_Lf; P-tot; O2-Saettigung; pH
#> 
#> ── (2/5)  Lese NAWA_Bsp_2.xlsx ... ─────────────────────────────────────────────
#> Warning: ! VSA-Lookup: 2 mehrfache Bezeichnungen (VSA Parameter-ID) pro Substanz_ID
#>   gefunden. Verwende tiefere Substanz_ID.
#> ℹ Betroffen: 8_2-FTCA, SiO2
#> ℹ Lese MV-Daten von Excel-Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_2.xlsx ein.
#> ℹ Versuche Start des Tabellen-Headers der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_2.xlsx zu erraten.
#> ✔ Erkannter Header-Start: Zeile 8.
#> ℹ Versuche Sprache der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_2.xlsx zu erraten.
#> ✔ Erkannte Sprache: DE.
#> ℹ Versuche Parameter-Feld der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_2.xlsx zu erraten.
#> ✔ Erkannter Parameter: BAFU_Parameter_ID.
#> ! Bestimmungsgrenzen wurden nicht als Zahlen eingelesen - versuche Typenkonvertierung.
#> ℹ Normalisiere Einheiten der MV-Daten auf µg/l.
#> Warning: ! NAWA_Bsp_2.xlsx: Duplikate für 130 Datensätze gefunden.
#> ℹ Es wird jeweils der Datensatz (pro Station, Beginn-/Enddatum-zeit und
#>   Substanz) mit dem höchsten Messwert verwendet.
#> ℹ Max./min. Bestimmungsgrenzen der MV-Daten bestimmen...
#> 
#> ── (3/5)  Lese NAWA_Bsp_3.csv ... ──────────────────────────────────────────────
#> Warning: ! VSA-Lookup: 2 mehrfache Bezeichnungen (VSA Parameter-ID) pro Substanz_ID
#>   gefunden. Verwende tiefere Substanz_ID.
#> ℹ Betroffen: 8_2-FTCA, SiO2
#> ℹ Lese MV-Daten von Text-Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_3.csv ein.
#> ℹ Versuche Encoding der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_3.csv zu erraten.
#> ✔ Erkanntes Encoding: UTF-8
#> ℹ Versuche Trennzeichen der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_3.csv zu erraten.
#> ✔ Erkanntes Trennzeichen: ;
#> ℹ Versuche Start des Tabellen-Headers der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_3.csv zu erraten.
#> ✔ Erkannter Header-Start: Zeile 1.
#> ℹ Versuche Sprache der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_3.csv zu erraten.
#> ✔ Erkannte Sprache: DE.
#> ℹ Lese NAWA-MV-Daten von /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_3.csv ein.
#> ℹ Versuche Parameter-Feld der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_3.csv zu erraten.
#> ✔ Erkannter Parameter: BAFU_Parameter_ID.
#> ℹ Normalisiere Einheiten der MV-Daten auf µg/l.
#> ℹ Max./min. Bestimmungsgrenzen der MV-Daten bestimmen...
#> Warning: ! /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_3.csv: Nicht alle
#>   Substanzen, die in den MV-Daten gefunden wurden, konnten einer VSA
#>   ID_Substanz zugeordnet werden. Daten ohne ID_Substanz oder PARAMETERID_BAFU
#>   werden entfernt.
#> ℹ 4 Fehlende Zuordnungen für 2-Hydroxy-propazin_und_2-Hydroxy-terbuthylazin;
#>   Acetochlor-ESA_und_Alachlor-ESA; Prometon_und_Terbumeton;
#>   Prometryn_und_Terbutryn
#> 
#> ── (4/5)  Lese NAWA_Bsp_4.xlsx ... ─────────────────────────────────────────────
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
#> 
#> ── (5/5)  Lese NAWA_Bsp_5.csv ... ──────────────────────────────────────────────
#> Warning: ! VSA-Lookup: 2 mehrfache Bezeichnungen (VSA Parameter-ID) pro Substanz_ID
#>   gefunden. Verwende tiefere Substanz_ID.
#> ℹ Betroffen: 8_2-FTCA, SiO2
#> ℹ Lese MV-Daten von Text-Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_5.csv ein.
#> ℹ Versuche Encoding der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_5.csv zu erraten.
#> ✔ Erkanntes Encoding: ISO-8859-1
#> ℹ Versuche Trennzeichen der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_5.csv zu erraten.
#> ✔ Erkanntes Trennzeichen: ;
#> ℹ Versuche Start des Tabellen-Headers der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_5.csv zu erraten.
#> ✔ Erkannter Header-Start: Zeile 1.
#> ℹ Versuche Sprache der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_5.csv zu erraten.
#> ✔ Erkannte Sprache: DE.
#> ℹ Lese NAWA-MV-Daten von /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_5.csv ein.
#> ℹ Versuche Parameter-Feld der Datei /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_5.csv zu erraten.
#> ✔ Erkannter Parameter: BAFU_Parameter_ID.
#> ℹ Normalisiere Einheiten der MV-Daten auf µg/l.
#> ℹ Max./min. Bestimmungsgrenzen der MV-Daten bestimmen...
#> ℹ Kombiniere MV-Daten aus 5 Dateien...
#> ✔ MV-Daten erfolgreich eingelesen und kombiniert.
```
