# Risikoquotienten (RQ) für Einzelstoffe berechnen

Berechnet Risikoquotienten für das akute (AQK) und das chronische (CQK)
Qualitätskriterium.

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

Dataframe mit MV-Daten, ergänzt mit Risikoquotienten. Enthält u.a.
`WERT_NUM_CQK` (Zweiwochenmischkonzentration), `Faktor_CQK`
(Hochrechnungsfaktor) und `Tage_CQK` (effektive Probendauer für die
chronische Beurteilung).

## Details

Für die chronische Beurteilung (andauernde Verunreinigung) wird eine
Zweiwochenmischkonzentration verwendet: Proben ab 13 Tagen gelten als
vollwertige Zweiwochenprobe (Toleranzbereich 13-14 Tage) und werden
direkt verwendet. Proben von 10 bis unter 13 Tagen werden
zeitproportional über 14 Tage (336 h) gemittelt, wobei die Konzentration
in der unbeprobten Zeit auf 0 gesetzt wird (Faktor Tage/14). Proben
unter 10 Tagen können nicht chronisch beurteilt werden (`RQ_CQK` =
`NA`). Die akute Beurteilung (`RQ_AQK`) basiert unverändert auf der
Rohkonzentration. Grundlage: Anhang 5.1 der Empfehlung zur Beurteilung
von Spurenstoffen (Anhang 2 GSchV).
