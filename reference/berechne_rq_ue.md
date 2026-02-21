# Risikoquotienten (RQ) für Einzelstoffe berechnen

Risikoquotienten (RQ) für Einzelstoffe berechnen

## Verwendung

``` r
berechne_rq_ue(
  mv_daten,
  regulierungen = NULL,
  kriterien = NULL,
  robust3 = TRUE
)
```

## Argumente

- mv_daten:

  Normalisierte MV-Daten

- regulierungen:

  Regulierungsdaten (dataframe)

- kriterien:

  Qualitätskriterien (dataframe)

- robust3:

  Logisch (Vorgabe: `TRUE`). Warnen, wenn QK mit Robustheit 3
  überschritten sind (nicht in Daten enthalten)?

## Rückgabewert

Dataframe mit MV-Daten, ergänzt mit Risikoquotienten
