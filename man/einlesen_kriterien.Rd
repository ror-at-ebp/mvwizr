% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsing.R
\name{einlesen_kriterien}
\alias{einlesen_kriterien}
\title{Qualitätskriterien einlesen}
\usage{
einlesen_kriterien(kriterien_pfad)
}
\arguments{
\item{kriterien_pfad}{Pfad zur Exceldatei mit den Qualitätskriterien}
}
\value{
Dataframe mit den Qualitätskriterien
}
\description{
Liest Qualitätskriterien ein. Bei mehreren Ökotoxikologischen Werten pro Substanz wird der tiefste Wert gewählt.
}
