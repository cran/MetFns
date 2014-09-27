\name{vmdbsite}
\alias{vmdbsite}
\docType{data}
\title{
List of observing sites
}
\description{
The data consists of a list of observing sites.
}
\usage{vmdbsite}
\format{
  A data frame with 2911 observations on the following 11 variables.
  \describe{
    \item{\code{Site}}{numeric IMO site code}
    \item{\code{Name}}{factor  Name of the site}
    \item{\code{Country}}{factor  Country of the observing site}
    \item{\code{long.deg}}{numeric  Degrees component of longitude of the observing site} 
    \item{\code{long.min}}{numeric  Minutes component of longitude of the observing site} 
    \item{\code{long.sec}}{numeric  Seconds component of longitude of the observing site} 
    \item{\code{ew}}{factor  East (E) or west (W) position from the prime meridian}
    \item{\code{lat.deg}}{numeric  Degrees component of latitude of the observing site}
    \item{\code{lat.min}}{numeric  Minutes component of latitude of the observing site}
    \item{\code{lat.sec}}{numeric  Seconds component of latitude of the observing site}
    \item{\code{ns}}{factor  North (N) or south (S) position from the equator}
  }
}
\source{
Visual Meteor Database, \url{http://www.imo.net/data/visual}
}
