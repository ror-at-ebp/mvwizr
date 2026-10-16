# mvwizr (Entwicklungsversion)

## Behobene Fehler & Änderungen

- Fehlerbehebung (#18): Die Datumsachse der Mischprobenplots (`plot_misch_verlauf`, `plot_misch_ue`) wurde in UTC statt in der Zeitzone der Daten (Europe/Zurich) erstellt. Proben mit Beginn am 01.01. um 00:00 lagen dadurch vor der Achsengrenze und fehlten im Plot.
- Fehlerbehebung: `einlesen_nawa()` (und damit `batch_einlesen_nawa()`) lieferte die Zeitstempel für CSV-Dateien und Excel-Dateien mit Datumszellen in UTC statt in Europe/Zurich. Die Uhrzeiten bleiben unverändert, neu wird aber konsistent die Zeitzone Europe/Zurich gesetzt (wie bei `einlesen_mv_gbl()`).
- Fehlerbehebung: Auf Systemen ohne installierte Schweizer Locale (`de_CH`/`fr_CH`, z.B. Linux-Server, Docker-Container) erzeugten Plots mit Zeitachse die Warnung "OS reports request to set locale ... cannot be honored" bzw. brachen bei `options(warn = 2)` ab. Die Monatsbeschriftungen weichen in diesem Fall nun ohne Warnung auf fest hinterlegte deutsche bzw. französische Abkürzungen aus. Ist die Locale vorhanden, bleibt das Verhalten unverändert.

# mvwizr 1.4.0

## Neue Features & Änderungen

- **Zweiwochenmischkonzentration für andauernde Verunreinigungen (Anh. 5.1 GSchV-Empfehlung):** Die Beurteilung andauernder Verunreinigungen (chronisches Qualitätskriterium CQK) erfolgt auf Basis von Zweiwochenmischkonzentrationen. Neu gelten erst Proben ab 13 Tagen als vollwertige Zweiwochenmischprobe (Toleranzbereich 13-14 Tage; zuvor ab 10 Tage). Proben von 10 bis unter 13 Tagen werden zeitproportional über 14 Tage (336 h) hochgerechnet, wobei die Konzentration in der unbeprobten Zeit auf 0 gesetzt wird. Proben unter 10 Tagen können chronisch nicht mehr beurteilt werden. Die frühere Schwelle von >= 10 Tagen wird damit durch >= 13 Tage (mit Hochrechnung für 10-13 Tage) ersetzt. Die akute Beurteilung (AQK) bleibt unverändert.
- **Mischungstoxizität "nicht bewertet" als Datenlücke:** In der Stationsübersicht Ökotox (`plot_misch_oekotox_uebersicht`) und im Mischtox-Verlauf (`plot_misch_mixtox_verlauf`) werden nicht bewertbare Ziele (z.B. sekundäre Ökotoxizität ohne gemessenes PFOS) nicht mehr grau dargestellt, sondern als Datenlücke ausgelassen. "nicht bewertet" erscheint nicht mehr als Legendeneintrag.
- **Häufigkeitsverteilung neu als Anzahl Wochen:** `plot_misch_mixtox_haeufigkeit` zeigt statt relativer Anteile (Prozent) neu die Anzahl bewerteter Wochen pro Jahr. Bei durchgehender Bewertung erreichen die Balken das Jahresmaximum; nicht bewertete Zeiträume ergeben kürzere Balken.
- Der Output von `berechne_rq_ue()` enthält neu die Spalten `WERT_NUM_CQK`, `Faktor_CQK` und `Tage_CQK`.

# mvwizr 1.3.3

## Behobene Fehler & Änderungen

- Fehlerbehebung: Falls der RQ-Wert genau 10 betrug, wurde keine Beurteilung erstellt durch die Funktionen `berechne_rq_ue` und `berechne_mixtox`.

# mvwizr 1.3.2

## Behobene Fehler

- Keine Änderungen für Benutzer
- Kleine Fehlerbehebungen und Codeverbesserungen
- Neue automatisierte Tests, die mehr Probleme abfangen
- Disclaimer: Claude Code wurde für dieses Release verwendet

# mvwizr 1.3.1

## Behobene Fehler & Änderungen

- Neu verfügt die Funktion `plot_misch_ue_qk(..., detailliert = TRUE)` über den zusätzlichen Parameter `pattern_key_scale`. Damit kann die Grösse der Schraffierung in der Legende angepasst werden. Falls in der Legende keine Schraffierung angezeigt wird, sollte dieser Wert reduziert werden (Vorgabe: 1).

# mvwizr 1.3.0

## Behobene Fehler & Änderungen

- Neu wird der Mischungstoxizitätstyp "Akkumulation" "Bioakkumulation" genannt.
- Die Paketwebseite enthält nun eine deutschsprachige Navigation.

# mvwizr 1.2.1

## Behobene Fehler

- Polychrome (getestet mit v1.5.4) liefert immer mindestens 5 Farben, was in `plot_misch_ue_qk(..., detailliert = TRUE)` zu einem Fehler führt, wenn weniger als 5 Substanzen im Datensatz vorhanden sind.

# mvwizr 1.2.0

## Neue Features

### Aktualisierte gebundelte Daten

-   Enthält die aktuell neueste Version der [Qualitätskriterien des Ökotoxzentrums](https://www.oekotoxzentrum.ch/expertenservice/qualitaetskriterien/qualitaetskriterienvorschlaege-oekotoxzentrum) (durch den VSA angepasst) - Stand Juni 2025
-   Enthält die aktuell neueste Version der BAFU-Liste Parameter-Bezeichnungen zum Dateiaustausch - Stand Dezember 2024
-   Enthält einen aktuellen Auszug der VSA Substanzenzenliste (für die Zuordnung von BAFU Parametername zu VSA Substanz ID) - Stand Juni 2025
-   Enthält eine aktuelle Version der VSA Tabelle Anforderungen Substanz Recht (Regulierungsinformationen) - Stand Juni 2025
-   Enthält neue Beispiel-MV-Daten `NAWA_Bsp_*` zum Prüfen der Funktionen `einlesen_nawa` und `batch_einlesen_nawa` und auch um die Verarbeitung von berechneten Mischproben zu testen
-   Enthält einen neuen Artikel/Vignette zum Umgang mit berechneten Mischproben bei NAWA-MV-Dateien (siehe [Paketwebseite](https://ror-at-ebp.github.io/mvwizr/))

### Neue Funktionalität

-   Drei neue Funktionen im Zusammenhang mit dem Verarbeiten von NAWA-MV-Dateien: `einlesen_nawa` (Einlesen einzelner Dateien), `batch_einlesen_nawa` (Einlesen mehrerer Dateien auf einmal) und `schreibe_nawa_import_manifest_template` als Hilfsfunktion für das Batch-Einlesen mit Manifest-Datei.

-   Eine neue exportierte Funktion `prozessiere_bSaP` zum Behandeln von Daten, die berechnete Mischproben enthalten (damit die Daten nicht doppelt bewertet werden) - auch für bereits eingelesene Datensätze. Dies ist für NAWA-MV-Daten relevant, da dort keine Möglichkeit spezifiziert ist, berechnete Mischproben ("bSaP") anzugeben. Wurde zuvor bereits in einfacher Version als interne Funktion von `einlesen_mv_gbl` verwendet.

-   Anpassungen an Plot-Funktionen: Bei Plots mit Mischungstoxizität kann nun mit dem Parameter `optin_mischtox_S` (Standard: `FALSE`) eine vierte Zeile eingeblendet werden, die die Mischungstoxizität bezüglich Secondary Poisoning zeigt. Die Zeile wird nur angezeigt, falls der Parameter entsprechend gesetzt wurde und Daten im Datensatz vorhanden sind, bei denen `S_chron = 1` in den Qualitätskriterien (zur Zeit nur PFOS).

-   Die volle Farbskala für die Ökotox-Bewertung wird nun immer angezeigt, auch wenn eine Bewertungsstufe (z.B. "sehr gut" - blau) im Plot fehlt. Zusätzlich wird nun in grau gezeigt, falls die Mischungstoxizität irgendwo nicht bewertet werden konnte.

-   Interne Anpassungen an der Funktion `einlesen_mv_gbl` ohne Auswirkungen auf die Funktionalität

-   Regulierungsinformationen und Qualitätskriterien müssen nun nicht mehr unbedingt angegeben werden. Falls das Argument in der Funktion `NULL` bleibt, verwendet mvwizr automatisch die gebundelte Version

## Behobene Fehler

-   Generell striktere Fehlerbehandlung (z.B. Prüfen auf leere Datensätze) und wo sinnvoll Fehlermeldungen mit Klasse
-   Unerwartetes Verhalten oder unerwünschte Resultate führen nun immer zu Warnungen (z.B. fehlende Zuordnungen BAFU-PARAMETER zu VSA SUbstanz-ID)

# mvwizr 1.1.0

-   Korrektur des Übertretungsplots plot_misch_ue_qk und weitere kleine Anpassungen

# mvwizr 1.0.0

-   Erste öffentliche Version von mvwizr
