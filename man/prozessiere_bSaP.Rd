% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsing.R
\name{prozessiere_bSaP}
\alias{prozessiere_bSaP}
\title{Entferne SaP, welche für bSaP verwendet wurden}
\usage{
prozessiere_bSaP(mv_daten, bSaP_identifier)
}
\arguments{
\item{mv_daten}{Tibble mit den MV-Daten aus der Funktion \code{einlesen_mv_gbl} oder \code{einlesen_nawa} resp. \code{batch_einlesen_nawa}.}

\item{bSaP_identifier}{Name der Spalte als String mit der Information, ob es sich um berechnete Mischproben ("bSaP") oder gemessene Mischproben ("SaP") handelt. Erlaubte Werte in Spalte: "bSaP", "SaP" und "S" (für Stichproben - werden nicht bearbeitet).}
}
\value{
mv-daten mit entfernten Datensätzen
}
\description{
Bei den (gemessenen) Mischproben unterscheidet das NAWA-MV-Protokoll zwischen 3.5-Tage-Proben und 14-Tage-Proben (SaP). Ausserdem können berechnete 14-Tage-Proben (bSaP) aus 4 kürzeren Mischproben errechnet werden. Damit diese Werte nicht doppelt gezählt werden, sollten solche kürzeren Proben, die für die Berechnung verwendet wurden, aus den Daten entfernt werden.
}
\details{
Für GBL-Daten wird diese Funktion automatisch aufgerufen. Da beim NAWA-MV-Format zur Zeit keine Kennzeichnung von bSaP-Proben vorgesehen ist, muss dieser Schritt nach dem Einlesen erfolgen, und bSaP Proben müssen manuell gekennzeichnet werden.

Achtung: Diese Berechnungen funktionieren nur, wenn die 3.5-Tage Proben zeitlich innerhalb der 14-Tage Proben liegen oder sich mit dem Zeitraum (auf Tag gerundet) decken. Falls die zur Berechnung verwendeten 3.5-Tage-Mischproben das Intervall der betreffenden bSaP 14-Tage-Probe überschneiden, so werden nur die 3.5-Tage-Proben, die innerhalb des Zeitraums liegen, entfernt!
}
\examples{
# Bei NAWA-MV-Dateien muss der bSaP-Identifier manuell hinzugefügt werden.
# Im folgenden Beispiel werden zwei Dateien eingelesen:
# - NAWA_ohne_bSaP_Bsp.xlsx enthält gemessene Mischproben (3.5d und 14d)
# - NAWA_mit_bSaP_Bsp.csv enthält die berechneten Mischproben

mv_bsap_pfade <- system.file("extdata", c(
  "NAWA_ohne_bSaP_Bsp.xlsx",
  "NAWA_mit_bSaP_Bsp.csv"
), package = "mvwizr")
out <- batch_einlesen_nawa(mv_bsap_pfade) |>
  dplyr::mutate(PROBEARTID = dplyr::if_else(.data$filename ==
    "NAWA_mit_bSaP_Bsp.csv", "bSaP", .data$PROBEARTID))

mv_bsap <- prozessiere_bSaP(out, bSaP_identifier = "PROBEARTID") |>
  dplyr::mutate(Dauer = difftime(.data$ENDEPROBENAHME,
    .data$BEGINNPROBENAHME,
    units = "days"
  ))

}
