# Schreibe Import Manifest Vorlage für NAWA-MV-Daten

Schreibe Import Manifest Vorlage für NAWA-MV-Daten

## Verwendung

``` r
schreibe_nawa_import_manifest_template(
  import_manifest_file,
  mv_datei_pfade = NULL
)
```

## Argumente

- import_manifest_file:

  Pfad zur Import-Manifest-Datei, die geschrieben werden soll. Diese
  Datei wird im Excel-Format (.xlsx) geschrieben.

- mv_datei_pfade:

  Dateipfade zu den MV-Daten, die im NAWA-Format vorliegen. Falls
  angegeben, werden alle Dateien in das Manifest aufgenommen. Falls
  `NULL`, wird ein leeres Manifest geschrieben.

## Rückgabewert

Gibt das Manifest-Template als tibble zurück.

## Beispiele

``` r
nawa_mv_pfade <- system.file("extdata", c(
  "NAWA_Bsp_1.xlsx",
  "NAWA_Bsp_2.xlsx", "NAWA_Bsp_3.csv", "NAWA_Bsp_4.xlsx",
  "NAWA_Bsp_5.csv"
), package = "mvwizr")

schreibe_nawa_import_manifest_template(tempfile(fileext = ".xlsx"),
  mv_datei_pfade = nawa_mv_pfade
)
#> ℹ Schreibe Import-Manifest für NAWA-MV-Daten in /tmp/RtmplICBvv/file26ad29025ee3.xlsx
#> ℹ Folgende Dateien werden dem Manifest hinzugefügt:
#> • /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_1.xlsx
#> • /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_2.xlsx
#> • /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_3.csv
#> • /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_4.xlsx
#> • /home/runner/work/_temp/Library/mvwizr/extdata/NAWA_Bsp_5.csv
```
