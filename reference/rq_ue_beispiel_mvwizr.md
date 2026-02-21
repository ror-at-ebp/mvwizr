# RQ und Ue aus MV-Beispieldaten

Dieser Datensatz beinhaltet Risikoquotienten und GSchV-Überschreitungen
zu den MV-Beispieldaten und wurde aus `mvdaten_beispiel_mvwizr`
berechnet.

## Verwendung

``` r
rq_ue_beispiel_mvwizr
```

## Format

### `rq_ue_beispiel_mvwizr`

Aufgrund des Joins der Eingangsdaten besteht dieser Datensatz aus 52
(oder mehr) Variablen, wobei viele davon bereits in der Hilfe zu den
Eingangsdaten beschrieben sind. Im Folgenden werden daher nur die
zusätzlich berechneten Variablen aufgeführt:

- RQ_CQK:

  Risikoquotient für CQK allgemein (numeric)

- RQ_AQK:

  Risikoquotient für AQK allgemein (numeric)

- RQ_CQK_P:

  Risikoquotient für CQK für Pflanzen (numeric)

- RQ_CQK_I:

  Risikoquotient für CQK für Invertebraten (numeric)

- RQ_CQK_V:

  Risikoquotient für CQK für Vertebraten (numeric)

- RQ_AQK_P:

  Risikoquotient für AQK für Pflanzen (numeric)

- RQ_AQK_I:

  Risikoquotient für AQK für Invertebraten (numeric)

- RQ_AQK_V:

  Risikoquotient für AQK für Vertebraten (numeric)

- Beurteilung_CQK:

  Beurteilung für CQK gem. MSK in 5 Stufen (factor)

- Beurteilung_AQK:

  Beurteilung für CQK gem. MSK in 5 Stufen (factor)

- Ue_anhaltend:

  Übertretung Grenzwert anhaltende Belastung? (logical)

- Ue_kurzzeitig:

  Übertretung Grenzwert kurzzeitige Belastung? (logical)

- Ue_spezifisch:

  Übertretung Grenzwert spezifische Belastungswerte (d.h. kurzzeitig
  oder spezifisch)? (logical)

- Ue_generisch:

  Übertretung generischer Grenzwert? (logical)

- Ue_AQK:

  Übertretung akutes QK (Robustheit 1,2)? (logical)

- Ue_CQK:

  Übertretung chronisches QK (Robustheit 1,2)? (logical)

## Quelle

GBL/VSA
