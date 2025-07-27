# mvwizr 1.2.0

## Neue Features

### Aktualisierte gebundelte Daten

* Enthält die aktuell neueste Version der [Qualitätskriterien des Ökotoxzentrums](https://www.oekotoxzentrum.ch/expertenservice/qualitaetskriterien/qualitaetskriterienvorschlaege-oekotoxzentrum) (durch den VSA angepasst) - Stand Juni 2025
* Enthält die aktuell neueste Version der BAFU-Liste Parameter-Bezeichnungen zum Dateiaustausch - Stand Dezember 2024
* Enthält einen aktuellen Auszug der VSA Substanzenzenliste (für die Zuordnung von BAFU Parametername zu VSA Substanz ID) - Stand Juni 2025
* Enthält eine aktuelle Version der VSA Tabelle Anforderungen Substanz Recht (Regulierungsinformationen) - Stand Juni 2025
* Enthält neue Beispiel-MV-Daten `NAWA_Bsp_*` zum Prüfen der Funktionen `einlesen_nawa` und `batch_einlesen_nawa`

### Neue Funktionalität

* Drei neue Funktionen im Zusammenhang mit dem Verarbeiten von NAWA-MV-Dateien: `einlesen_nawa` (Einlesen einzelner Dateien), `batch_einlesen_nawa` (Einlesen mehrerer Dateien auf einmal) und `schreibe_nawa_import_manifest_template` als Hilfsfunktion für das Batch-Einlesen mit Manifest-Datei.
* Eine neue exportierte Funktion `prozessiere_bSaP` zum Behandeln von Daten, die berechnete Mischproben enthalten (damit die Daten nicht doppelt bewertet werden) - auch für bereits eingelesene Datensätze. Dies ist für NAWA-MV-Daten relevant, da dort keine Möglichkeit spezifiziert ist, berechnete Mischproben ("bSaP") anzugeben. Wurde zuvor bereits in einfacher Version als interne Funktion von `einlesen_mv_gbl` verwendet.
* Neue Funktion zum Entfernen von Du

* Interne Anpassungen an der Funktion `einlesen_mv_gbl` ohne Auswirkungen auf die Funktionalität


## Behobene Fehler

* Generell striktere Fehlerbehandlung (z.B. Prüfen auf leere Datensätze) und wo sinnvoll Fehlermeldungen mit Klasse
* Unerwartetes Verhalten oder unerwünschte Resultate führen nun immer zu Warnungen (z.B. fehlende Zuordnungen BAFU-PARAMETER zu VSA SUbstanz-ID)


# mvwizr 1.1.0

* Korrektur des Übertretungsplots plot_misch_ue_qk und weitere kleine Anpassungen

# mvwizr 1.0.0

* Erste öffentliche Version von mvwizr
