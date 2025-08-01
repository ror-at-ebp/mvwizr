% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{plot_misch_mixtox_haeufigkeit}
\alias{plot_misch_mixtox_haeufigkeit}
\title{Zeitreihe Häufigkeitsverteilung Mischungstoxizität}
\usage{
plot_misch_mixtox_haeufigkeit(
  rq_ue_daten,
  stationscode,
  modus = c("andauernd", "kurzzeitig"),
  optin_mischtox_S = FALSE
)
}
\arguments{
\item{rq_ue_daten}{Dataframe mit Output der Funktion \code{berechne_rq_ue()}}

\item{stationscode}{Station, für welche der Plot erstellt werden soll.}

\item{modus}{\itemize{
\item \code{"andauernd"}: Berücksichtigt nur Stoffe mit einem spezifischen andauernden Grenzwert in der GSchV. Nur Proben mit einer Dauer von >= 10 Tagen werden verwendet und unter Verwendung des CQK beurteilt.
\item \code{"kurzzeitig"}: Berücksichtigt nur Stoffe mit einem spezifischen akuten Grenzwert in der GSchV. Berücksichtigt alle Proben (ausser Stichproben) und verwendet das AQK zur Beurteilung.
}}

\item{optin_mischtox_S}{Logisch (Vorgabe: \code{FALSE}). Ab v1.2 unterstützt mvwizr auch die Anzeige der Akkumulation / Secondary toxicity (\code{S_chron}). Falls \code{TRUE}, wird eine vierte Zeile bei den Mischtoxizitäten angezeigt, falls sie in den Daten vorhanden ist. Falls \code{FALSE}, wird sie nicht angezeigt.}
}
\value{
ggplot2 Plot-Objekt
}
\description{
Zeigt die Häufigkeitsverteilung der Beurteilung der Mischungstoxizität über die Zeit für eine Station.
}
\examples{
# Häufigkeitsverteilung für andauernde Belastungen
plot_misch_mixtox_haeufigkeit(rq_ue_beispiel_mvwizr,
  stationscode = "URT010",
  modus = "andauernd"
)

# Häufigkeitsverteilung für kurzzeitige Belastungen
plot_misch_mixtox_haeufigkeit(rq_ue_beispiel_mvwizr,
  stationscode = "URT010",
  modus = "kurzzeitig"
)
}
