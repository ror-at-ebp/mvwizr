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
