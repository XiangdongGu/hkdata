% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hk-transport.R
\name{transport_Flight_retrieve}
\alias{transport_Flight_retrieve}
\title{Generate the Flight Information given the date, arrival or departure, cargo or passenger for different language versions}
\usage{
transport_Flight_retrieve(Date, Arrival, Cargo, Lang = "en")
}
\arguments{
\item{Date}{String, "YYYY-MM-DD" (The data is up-to-date to the previous calendar day.)}

\item{Arrival}{Arrival or departure (TRUE=arrival, FALSE=departure)}

\item{Cargo}{Cargo or passenger flight (TRUE=cargo, FALSE=passenger)}

\item{Lang}{Options of Language: "zh_HK": traditional Chinese; "zh_CN": simplified Chinese; "en": English (default)}
}
\description{
Generate the Flight Information given the date, arrival or departure, cargo or passenger for different language versions
}
