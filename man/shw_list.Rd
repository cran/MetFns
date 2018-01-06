\name{shw_list}
\alias{shw_list}
\docType{data}
\title{
List of meteor showers
}
\description{
The data consists of a list of visual meteor showers.
}
\usage{shw_list}
\format{
  A data frame with 38 observations on the following 11 variables.
  \describe{
    \item{\code{Shw}}{factor  Three-letter shower code}
    \item{\code{Name}}{factor  Shower name}
    \item{\code{Activity.beg}}{factor  The beginning of the activity period}
    \item{\code{Activity.end}}{factor  The end of the activity period}
    \item{\code{Max}}{factor  The date of maximum activity}
    \item{\code{Sollong}}{numeric  Solar longitude of the date of maximum}
    \item{\code{Alpha}}{numeric  Right ascension of radiant at date of maximum}
    \item{\code{Delta}}{numeric  Declination of radiant at date of maximum}
    \item{\code{V}}{numeric  Geocentric velocity of the stream}
    \item{\code{r}}{numeric  Population index of a meteor shower}
    \item{\code{ZHR}}{numeric Zenithal Hourly Rate of meteor shower during maximum activity}
  }
}
\source{
2018 Meteor Shower Calendar, \url{http://www.imo.net/files/meteor-shower/cal2018.pdf}
}
\note{Data is taken from Table 5. Working List of Visual Meteor Showers.
\emph{Details in this Table were correct
according to the best information available in June 2017, with maximum
dates accurate only for 2018.}}

