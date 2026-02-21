# Aufsummierte QK-Überschreitungen plotten

Zeigt die Überschreitung chronischer oder akuter Qualitätskriterien pro
Jahr und Station, wobei sichtbar wird, ob es viele relevante
Überschreitungen gibt von Qualitätskriterien, die nicht in der GSchV
aufgenommen sind als spezifischer Grenzwert.

## Verwendung

``` r
plot_misch_ue_qk(
  rq_ue_daten,
  stationscode = NULL,
  jahr = NULL,
  qk = c("chronisch", "akut"),
  detailliert = FALSE,
  pattern_key_scale = 1
)
```

## Argumente

- rq_ue_daten:

  Dataframe mit Output der Funktion
  [`berechne_rq_ue()`](https://ror-at-ebp.github.io/mvwizr/reference/berechne_rq_ue.md)

- stationscode:

  Station, für welche der Plot erstellt werden soll.

- jahr:

  Jahr, für welches der Überschreitungsplot erstellt werden soll.

- qk:

  Qualitätskriterium, für welches geplottet werden soll. Mögliche Werte:
  "chronisch" (Vorgabe) oder "akut".

- detailliert:

  Logisch (Vorgabe: `FALSE`). Soll der detaillierte VSA-Plot mit
  Aufspaltung nach Substanzen erstellt werden?

- pattern_key_scale:

  Skalierungsfaktor (Vorgabe: 1), der angibt, wie das Muster in den
  Legendeneinträgen skaliert werden soll (nur für `detailliert = TRUE`).
  Diesen Wert reduzieren, falls ein Muster im Plot gezeigt wird, aber
  nicht in der Legende.

## Rückgabewert

ggplot2 Plot-Objekt

## Beispiele

``` r
# Chronische Übertretungen auswerten
plot_misch_ue_qk(rq_ue_beispiel_mvwizr, qk = "chronisch", detailliert = FALSE)


# Akute Übertretungen auswerten
plot_misch_ue_qk(rq_ue_beispiel_mvwizr, qk = "akut", detailliert = FALSE)


# \donttest{
# Chronische Übertretungen detailliert auswerten (VSA-Grafik)
# Wird nicht automatisch getestet, weil Run zu lange dauert
plot_misch_ue_qk(rq_ue_beispiel_mvwizr, qk = "chronisch", detailliert = TRUE)

# }
```
