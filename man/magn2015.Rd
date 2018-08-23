\name{magn2015}
\alias{magn2015}
\docType{data}
\title{
Magnitude data for the year 2015
}
\description{
Visual meteor magnitude data for the year 2015.
}
\usage{magn2015}
\format{
  A data frame with 9264 observations on the following 32 variables.
  \describe{
    \item{\code{Magnitude.ID}}{numeric Magnitude ID number}
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
    \item{\code{P}}{numeric  Percentage of field-of-view obstruction}
    \item{\code{F}}{numeric  Correction factor for field-of-view obstruction}
    \item{\code{Lmg}}{numeric  Limiting magnitude}
    \item{\code{Shower}}{character  Meteor shower three-letter code}
    \item{\code{Mag.N6}}{numeric  Number of observed meteors of -6 magnitude}  
    \item{\code{Mag.N5}}{numeric  Number of observed meteors of -5 magnitude}
    \item{\code{Mag.N4}}{numeric  Number of observed meteors of -4 magnitude}
    \item{\code{Mag.N3}}{numeric  Number of observed meteors of -3 magnitude}
    \item{\code{Mag.N2}}{numeric  Number of observed meteors of -2 magnitude}
    \item{\code{Mag.N1}}{numeric  Number of observed meteors of -1 magnitude}
    \item{\code{Mag.0}}{numeric  Number of observed meteors of 0 magnitude}
    \item{\code{Mag.1}}{numeric  Number of observed meteors of +1 magnitude}
    \item{\code{Mag.2}}{numeric  Number of observed meteors of +2 magnitude}
    \item{\code{Mag.3}}{numeric  Number of observed meteors of +3 magnitude}
    \item{\code{Mag.4}}{numeric  Number of observed meteors of +4 magnitude}
    \item{\code{Mag.5}}{numeric  Number of observed meteors of +5 magnitude}
    \item{\code{Mag.6}}{numeric  Number of observed meteors of +6 magnitude}
    \item{\code{Mag.7}}{numeric  Number of observed meteors of +7 magnitude}
    \item{\code{Number}}{numeric  Total number of observed meteors}
  }
}
\source{
IMO Visual Meteor Database, \url{https://www.imo.net/members/imo_vmdb}
}
