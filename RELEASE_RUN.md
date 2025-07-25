# Schritte vor dem PR/Merge nach main

Da der neueste Commit auf dem main-Branch automatisch die release-Version von mvwizr ist, kann nicht direkt nach main gepusht werden. Um einen PR nach main zu mergen, müssen gewisse Bedingungen erfüllt sein. Im folgenden werden diese ausgeführt.

Zuerst folgende Checkliste durchgehen:

-   Sind alle exportieren Funktionen dokumentiert? Muss etwas ergänzt werden seit der letzten Version?
-   Sind Vignetten aktuell resp. müssen neue Vignetten ergänzt werden (erscheinen auf Paket-Webseite)? Vignetten lokal prüfen.
-   Gibt es Beispiele für alle relevanten Funktionen?
-   Sind Contributors in der `AUTHORS.md` verdankt und Email-Adressen aktuell?
-   Sind keine sensitiven Daten (Messdaten, Secrets) im Commit?
-   Gibt es in neuen/veränderten Funktionen ausreichende Fehlerbehandlung, insbesondere bei I/O-Operationen?
-   Gibt es relevante `testthat` Tests, die die wichtigsten Szenarien abdecken? Werden Grafiken ebenfalls mittels `vdiffr` Tests (lokal - deaktivert auf CI) getestet?
-   Ist das `README.Rmd` aktuell?

Falls dies alles erfüllt ist, dann hier weiterfahren:

1.  Sicherstellen, dass devtools und Paket geladen sind: RStudio Projekt muss geöffnet sein, dann in RStudio Build -\> Load All

2.  Gebundelte Daten im Package aktualisieren:

``` r
source("data-raw/mvwizr_alle_daten.R")
```

3.  Tests mit Testthat laufen lassen: In RStudio Build -\> Test Package

4.  Falls Probleme auftauchen, diese beheben und wieder zu 1.

5.  Falls keine Probleme auftauchen, als nächstes alles dokumentieren: In RStudio Build -\> Document

6.  Dann das `README.Rmd` rendern:

``` r
devtools::build_readme()
```

7.  Falls keine Probleme auftauchen, den `R CMD CHECK` lokal laufen lassen: In RStudio Build -\> Check Package

8.  Paket-Version erhöhen (in der Regel "minor"[^release_run-1])

[^release_run-1]: Semantic versioning gemäss dem Buch [R Packages](https://r-pkgs.org/lifecycle.html#sec-lifecycle-release-type). Für rückwärtskompatible Änderungen "minor" oder bei kleinen Änderungen "patch". Auf developer branch "dev" (z.B. 1.1.0.9000).

``` r
usethis::use_version(which = "minor")
```

9.  Eintrag in `NEWS.md` ergänzen - aus Nutzerperspektive beschreiben

10. Commit, push nach Entwickler-Branch und dann Pull Request eröffnen. Der Pull Request wird nur genehmigt, wenn der `R CMD CHECK` via github Actions fehlerfrei läuft.

11. Nach dem Merge mit main wird automatisch die Paket-Webseite aktualisiert.
