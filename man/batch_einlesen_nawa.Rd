% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsing.R
\name{batch_einlesen_nawa}
\alias{batch_einlesen_nawa}
\title{Batch-Einlesen von NAWA-MV-Daten}
\usage{
batch_einlesen_nawa(
  nawa_mv_pfade = NULL,
  vsa_lookup_pfad = NULL,
  bafu_lookup_pfad = NULL,
  import_manifest = NULL
)
}
\arguments{
\item{nawa_mv_pfade}{Dateipfade zu MV-Daten im NAWA-Format (Excel oder Text). Falls \code{NULL}, muss das Import-Manifest verwendet werden.}

\item{vsa_lookup_pfad}{Pfad zur VSA-Tabelle \code{Tab_Substanzen.xlsx}}

\item{bafu_lookup_pfad}{Pfad zum BAFU-Namen-Lookup}

\item{import_manifest}{Pfad zu einem Import-Manifest, das die MV-Daten-Dateien und deren Parameter enthält. Falls \code{NULL}, müssen die Dateipfade bei \code{nawa_mv_pfade} angegeben werden und alle Parameter werden geraten. Das Manifest muss eine Excel-Datei sein, die die Spalten \code{file}, \code{encoding}, \code{header}, \code{delimiter} und \code{lang} enthält. Mittels \code{schreibe_nawa_import_manifest_template()} kann ein leeres Manifest erstellt werden, das dann mit den entsprechenden Dateien gefüllt werden kann.}
}
\value{
Dataframe mit kombinierten MV-Daten aus allen angegebenen Dateien (Achtung: Entfernt keine Duplikate zwischen den Dateien, sondern nur innerhalb einer Datei).
}
\description{
Liest mehrere NAWA-MV-Daten-Dateien ein, entweder über ein Import-Manifest oder eine Liste von Dateipfaden. Die Funktion kann auch die Parameter wie Encoding, Header, Delimiter und Sprache erraten, wenn diese nicht angegeben sind (oder nur teilweise).
}
\examples{
# Batch-Einlesen mit Heuristik
# Die Zahlreichen Meldungen der Funktion sind hilfreich beim Auffinden von Problemen

nawa_mv_pfade <- system.file("extdata", c(
  "NAWA_Bsp_1.xlsx",
  "NAWA_Bsp_2.xlsx", "NAWA_Bsp_3.csv", "NAWA_Bsp_4.xlsx",
  "NAWA_Bsp_5.csv"
), package = "mvwizr")

out <- batch_einlesen_nawa(nawa_mv_pfade)

}
