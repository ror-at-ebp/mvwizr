# MV-Beispieldaten des Kantons Bern

MV-Daten bestehend aus mehreren Datensätzen von unterschiedlichen
Stationen für die Jahre 2019 und 2020. Beinhaltet Stichproben,
3.5-Tage-Proben und Proben mit längerem Sampling-Intervall. Folgende
wichtige Punkte sollten beachtet werden:

- Die Einheiten wurden alle auf µg/l normalisiert

- Duplikate (unterschiedliche Werte für dieselbe Probe und Substanz)
  wurden (konservativ) entfernt

- Einträge mit fehlender Substanz_ID oder PARAMETERID_BAFU wurden
  entfernt.

## Verwendung

``` r
mvdaten_beispiel_mvwizr
```

## Format

### `mvdaten_beispiel_mvwizr`

Eine Tabelle bestehend aus 22 Variablen ca. 100k
Beobachtungen/Messungen. Untenstehend sind alle Variablen mit ihren
Datentypen beschrieben:

- UID:

  Eindeutiger Bezeichner für Messung (integer)

- CODE:

  Stationscode (character)

- STANDORT:

  Standort Station (character)

- NAME:

  Name des Gewässers (character)

- PROBEARTID:

  Art der Probe: Stichprobe (S), Mischprobe (SaP) oder berechnete
  Mischprobe (bSaP) (character)

- BEGINNPROBENAHME:

  Beginn der Probenahme (POSIXct; Locale Europe/Zurich)

- ENDEPROBENAHME:

  Ende der Probenahme (SaP) resp. leer (S) (POSIXct; Locale
  Europe/Zurich)

- ID_Substanz:

  Gematchte VSA-ID der Substanz (integer)

- PARAMETERID_BAFU:

  BAFU-Schlüssel für Substanz (character)

- OPERATOR:

  Falls vorhanden (\<) liegt Wert unter Bestimmungsgrenze (BG)
  (character)

- WERT_NUM:

  Konzentrationswert in µg/l. Falls 0, liegt Wert unter BG (numeric)

- Konz_inkl_BG:

  Ursprünglicher Konzentrationswert zur Kontrolle, ggf. unter BG
  (numeric)

- EINHEIT:

  Einheit der Messung. Hier immer µg/l normalisiert (character)

- MSTLTYP:

  Messstellentyp (character)

- PARAMETERGRUPPEID:

  Parametergruppencode des GBL (integer)

- PARAMETERGRUPPE:

  Bezeichnung der Parametergruppe des GBL (character)

- PARAMETER:

  Stoffbezeichnung des GBL intern (character)

- BEZEICHNUNG_BAFU:

  Deutsche BAFU-Bezeichnung aus GBL Datenbank (character)

- BAFU_Bez_DE:

  Deutsche Bezeichnung gemäss Matching mit BAFU Lookup (character)

- BAFU_Bez_FR:

  Französische Bezeichnung gemäss Matching mit BAFU Lookup (character)

- BG_max:

  Maximale festgestellte Bestimmungsgrenze für Substanz aus Daten in
  µg/l (numeric)

- BG_min:

  Minimale festgestellte Bestimmungsgrenze für Substanz aus Daten in
  µg/l (numeric)

## Quelle

GBL Kt. Bern
