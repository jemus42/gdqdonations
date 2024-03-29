% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gdq-tracker-donations.R
\name{get_donations}
\alias{get_donations}
\title{Get all donations for an event from the tracker}
\usage{
get_donations(event = "latest", delay = 0.5)
}
\arguments{
\item{event}{\verb{["latest"]} Event such as \code{"AGDQ2021"}, case insensitive. The default,
\code{"latest"}, is an alias for \verb{[latest_event()]}.}

\item{delay}{\verb{[0.5]}: Seconds to wait between pages. Don't annoy the webserver.}
}
\value{
A \link[tibble:tibble]{tibble}.
}
\description{
Get all donations for an event from the tracker
}
\examples{
\dontrun{
donations_sgdq2021 <- get_donations(event = "sgdq2021")
}
}
