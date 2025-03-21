% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsing.R
\name{berechne_stichproben_gbl_aggregiert}
\alias{berechne_stichproben_gbl_aggregiert}
\title{Stichprobenauswertung GBL}
\usage{
berechne_stichproben_gbl_aggregiert(mv_daten, perzentil = 90)
}
\arguments{
\item{mv_daten}{Tibble mit den MV-Daten aus der Funktion \code{einlesen_mv_gbl} resp gemäss Spezifikationen.}

\item{perzentil}{Perzentil, das berechnet werden soll pro Substanz (Vorgabe: 90).}
}
\value{
Tibble mit aggregierten Stichproben
}
\description{
Wertet Stichproben nach Parametergruppe aus. Dazu wird pro Station und Jahr das Perzentil (Vorgabe: 90) der Werte pro Substanz berechnet. Danach wird die Summe der Perzentile pro Parametergruppe berechnet und zurückgegeben.
}
