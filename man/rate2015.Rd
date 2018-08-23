\name{rate2015}
\alias{rate2015}
\docType{data}
\title{
Rate data for the year 2015
}
\description{
Visual meteor rate data for the year 2015.
}
\usage{rate2015}
\format{
  A data frame with 27408 observations on the following 21 variables.
  \describe{
    \item{\code{Rate.ID}}{numeric Rate ID number}
    \item{\code{Obs.Session.ID}}{numeric  Observing session ID number}
    \item{\code{User.ID}}{numeric  User ID number}
    \item{\code{First.Name}}{character  Observer's first name}
    \item{\code{Last.Name}}{character  Observer's last name}
    \item{\code{City}}{character  Observing site}
    \item{\code{Country}}{character  Observing country}
    \item{\code{Latitude}}{numeric  Latitude of the observing site, from -90 to 90 degrees}
    \item{\code{Longitude}}{numeric  Longitude of the observing site, from -180 to 180 degrees}
    \item{\code{Elevation}}{numeric  Elevation above sea level of observing site, in metres}
    \item{\code{Start.Date}}{POSIXct  Beginning of the observing time interval, UTC timezone}
    \item{\code{End.Date}}{POSIXct  End of the observing time interval, UTC timezone}
    \item{\code{Sollong}}{numeric  Solar longitude of the middle of observing time period}
    \item{\code{Ra}}{numeric  Right ascension of the center of the field of view, from 0 to 360 degrees}
    \item{\code{Decl}}{numeric  Declination of the center of the field of view, from -90 to 90 degrees}
    \item{\code{Teff}}{numeric  Effective observing time}
    \item{\code{P}}{numeric  Percentage of field-of-view obstruction}
    \item{\code{F}}{numeric  Correction factor for field-of-view obstruction}
    \item{\code{Lmg}}{numeric  Limiting magnitude}
    \item{\code{Shower}}{character  Meteor shower three-letter code} 
    \item{\code{Number}}{numeric  Number of meteors belonging to the observed shower}
  }
}
\source{
IMO Visual Meteor Database, \url{https://www.imo.net/members/imo_vmdb}
}

