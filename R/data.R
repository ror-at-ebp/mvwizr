#' MV-Beispieldaten des Kantons Bern
#'
#' MV-Daten bestehend aus mehreren Datensätzen von unterschiedlichen Stationen für die Jahre 2019 und 2020.
#' Beinhaltet Stichproben, 3.5-Tage-Proben und Proben mit längerem Sampling-Intervall.
#' Folgende wichtige Punkte sollten beachtet werden:
#' * Die Einheiten wurden alle auf µg/l normalisiert
#' * Duplikate (unterschiedliche Werte für dieselbe Probe und Substanz) wurden (konservativ) entfernt
#' * Einträge mit fehlender Substanz_ID oder PARAMETERID_BAFU wurden entfernt.
#'
#' @format ## `mvdaten_beispiel_mvwizr`
#' Eine Tabelle bestehend aus 22 Variablen ca. 100k Beobachtungen/Messungen. Untenstehend sind alle Variablen mit ihren Datentypen beschrieben:
#' \describe{
#'   \item{UID}{Eindeutiger Bezeichner für Messung (integer)}
#'   \item{CODE}{Stationscode (character)}
#'   \item{STANDORT}{Standort Station (character)}
#'   \item{NAME}{Name des Gewässers (character)}
#'   \item{PROBEARTID}{Art der Probe: Stichprobe (S), Mischprobe (SaP) oder berechnete Mischprobe (bSaP) (character)}
#'   \item{BEGINNPROBENAHME}{Beginn der Probenahme (POSIXct; Locale Europe/Zurich)}
#'   \item{ENDEPROBENAHME}{Ende der Probenahme (SaP) resp. leer (S) (POSIXct; Locale Europe/Zurich)}
#'   \item{ID_Substanz}{Gematchte VSA-ID der Substanz (integer)}
#'   \item{PARAMETERID_BAFU}{BAFU-Schlüssel für Substanz (character)}
#'   \item{OPERATOR}{Falls vorhanden (<) liegt Wert unter Bestimmungsgrenze (BG) (character)}
#'   \item{WERT_NUM}{Konzentrationswert in µg/l. Falls 0, liegt Wert unter BG (numeric)}
#'   \item{Konz_inkl_BG}{Ursprünglicher Konzentrationswert zur Kontrolle, ggf. unter BG (numeric)}
#'   \item{EINHEIT}{Einheit der Messung. Hier immer µg/l normalisiert (character)}
#'   \item{MSTLTYP}{Messstellentyp (character)}
#'   \item{PARAMETERGRUPPEID}{Parametergruppencode des GBL (integer)}
#'   \item{PARAMETERGRUPPE}{Bezeichnung der Parametergruppe des GBL (character)}
#'   \item{PARAMETER}{Stoffbezeichnung des GBL intern (character)}
#'   \item{BEZEICHNUNG_BAFU}{Deutsche BAFU-Bezeichnung aus GBL Datenbank (character)}
#'   \item{BAFU_Bez_DE}{Deutsche Bezeichnung gemäss Matching mit BAFU Lookup (character)}
#'   \item{BAFU_Bez_FR}{Französische Bezeichnung gemäss Matching mit BAFU Lookup (character)}
#'   \item{BG_max}{Maximale festgestellte Bestimmungsgrenze für Substanz aus Daten in µg/l (numeric)}
#'   \item{BG_min}{Minimale festgestellte Bestimmungsgrenze für Substanz aus Daten in µg/l (numeric)}
#' }
#'
#' @source GBL Kt. Bern
"mvdaten_beispiel_mvwizr"

#' Qualitätskriterien
#'
#' Dieser Datensatz beinhaltet Qualitätskriterien des VSA zu Mikroverunreinigungen (ohne Duplikate).
#'
#' @format ## `kriterien_mvwizr`
#' Eine Tabelle mit 10 Variablen und 441 Einträgen. Untenstehend sind alle Variablen mit ihren Datentypen beschrieben:
#' \describe{
#'   \item{ID_Substanz}{VSA-ID der Substanz (integer)}
#'   \item{P_chron}{Gibt an, ob CQK zu berücksichtigen für Mischtoxizität bei Pflanzen (integer)}
#'   \item{I_chron}{Gibt an, ob CQK zu berücksichtigen für Mischtoxizität bei Invertebraten (integer)}
#'   \item{V_chron}{Gibt an, ob CQK zu berücksichtigen für Mischtoxizität bei Vertebraten (integer)}
#'   \item{P_akut}{Gibt an, ob AQK zu berücksichtigen für Mischtoxizität bei Pflanzen (integer)}
#'   \item{I_akut}{Gibt an, ob AQK zu berücksichtigen für Mischtoxizität bei Invertebraten (integer)}
#'   \item{V_akut}{Gibt an, ob AQK zu berücksichtigen für Mischtoxizität bei Vertebraten (integer)}
#'   \item{Robustheit QK}{Robustheit des QK. Wert 1-3, wobei QK mit einem Wert von 3 nicht verwendet werden (integer)}
#'   \item{CQK}{Chronisches Qualitätskriterium in µg/l (numeric)}
#'   \item{AQK}{Akutes Qualitätskriterium in µg/l (numeric)}
#' }
#'
#' @source VSA
"kriterien_mvwizr"

#' Regulierungen
#'
#' Dieser Datensatz beinhaltet Informationen zur Regulierung von Mikroverunreinigungen.
#'
#' @format ## `regulierungen_mvwizr`
#' Tabelle mit 4 Variablen und knapp 3000 Einträgen. Im Folgenden sind die Variablen mit ihren Datentypen beschrieben:
#' \describe{
#'   \item{ID_Substanz}{VSA-ID der Substanz (integer)}
#'   \item{Name_reg}{Bezeichnung, die vom VSA verwendet wird (character)}
#'   \item{Informationen Recht}{Zeichenkette mit codierten Regulierungsinformationen (character)}
#'   \item{GSCHV}{Gibt an, ob und wie die Substanz in der GSCHV reguliert ist. 1 = Spezifischer Wert in Anh. 2; 2 = allgemeiner Grenzwert von 0.1 µg/l (integer)}
#' }
#'
#' @source VSA
"regulierungen_mvwizr"

#' RQ und Ue aus MV-Beispieldaten
#'
#' Dieser Datensatz beinhaltet Risikoquotienten und GSchV-Überschreitungen zu den MV-Beispieldaten und wurde aus `mvdaten_beispiel_mvwizr` berechnet.
#'
#' @format ## `rq_ue_beispiel_mvwizr`
#' Aufgrund des Joins der Eingangsdaten besteht dieser Datensatz aus 52 (oder mehr) Variablen, wobei viele davon bereits in der Hilfe zu den Eingangsdaten beschrieben sind. Im Folgenden werden daher nur die zusätzlich berechneten Variablen aufgeführt:
#' \describe{
#'   \item{RQ_CQK}{Risikoquotient für CQK allgemein (numeric)}
#'   \item{RQ_AQK}{Risikoquotient für AQK allgemein (numeric)}
#'   \item{RQ_CQK_P}{Risikoquotient für CQK für Pflanzen (numeric)}
#'   \item{RQ_CQK_I}{Risikoquotient für CQK für Invertebraten (numeric)}
#'   \item{RQ_CQK_V}{Risikoquotient für CQK für Vertebraten (numeric)}
#'   \item{RQ_AQK_P}{Risikoquotient für AQK für Pflanzen (numeric)}
#'   \item{RQ_AQK_I}{Risikoquotient für AQK für Invertebraten (numeric)}
#'   \item{RQ_AQK_V}{Risikoquotient für AQK für Vertebraten (numeric)}
#'   \item{Beurteilung_CQK}{Beurteilung für CQK gem. MSK in 5 Stufen (factor)}
#'   \item{Beurteilung_AQK}{Beurteilung für CQK gem. MSK in 5 Stufen (factor)}
#'   \item{Ue_anhaltend}{Übertretung Grenzwert anhaltende Belastung? (logical)}
#'   \item{Ue_kurzzeitig}{Übertretung Grenzwert kurzzeitige Belastung? (logical)}
#'   \item{Ue_spezifisch}{Übertretung Grenzwert spezifische Belastungswerte (d.h. kurzzeitig oder spezifisch)? (logical)}
#'   \item{Ue_generisch}{Übertretung generischer Grenzwert? (logical)}
#'   \item{Ue_AQK}{Übertretung akutes QK (Robustheit 1,2)? (logical)}
#'   \item{Ue_CQK}{Übertretung chronisches QK (Robustheit 1,2)? (logical)}
#' }
#'
#' @source GBL/VSA
"rq_ue_beispiel_mvwizr"
