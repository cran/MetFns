\name{filter.datetime}
\alias{filter.datetime}
\title{
Selection of visual meteor data by date(s)
}
\description{
Selects data for a given visual meteor dataset and specified year, month (s), day (s) and time period.
}
\usage{
filter.datetime(data,year,month.beg,month.end=month.beg,day.beg,
day.end = day.beg,time.beg=0,time.end=2359)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{year}{
numeric vector of length 4 specifying year.
}
  \item{month.beg}{
numeric vector specifying the beginning month.
}
  \item{month.end}{
numeric vector specifying the ending month. By default, \code{month.end} is set to be equal to \code{month.beg}.
}
  \item{day.beg}{
numeric vector specifying the beginning day.
}
  \item{day.end}{
numeric vector specifying the ending day. By default, \code{day.end} is set to be equal to \code{day.beg}.
}
  \item{time.beg}{
numeric vector (0-2359) specifying lower boundary of time in hours and minutes, corresponding to \code{day.beg}. By default, \code{time.beg} is set to be equal to 0.
}
  \item{time.end}{
numeric vector(0-2359) specifying upper boundary of time in hours and minutes, corresponding to \code{day.end}. By default, \code{time.end} is set to be equal to 2359.
}
}
\details{
Day given in meteor datasets corresponds to the beginning of the observing time period. In selection of 
the data, day corresponding to the middle of the observing time period is used.

If argument \code{day.end} is not provided, the function \code{filter.datetime} selects data for a given date, 
otherwise it selects data for a period of days, bounded by \code{day.beg} and \code{day.end}, in one or more months, bounded by \code{month.beg} and \code{month.end}. Arguments \code{time.beg} and 
\code{time.end} correspond to \code{day.beg} and \code{day.end}, respectively. If they are provided, the middle of the observing time period is used in data selection.
}

\value{
\code{filter.datetime} returns data frame with the same number of columns as the argument \code{data},
containing observations which correspond to the specified time period, date or period of days.
}
\references{
\url{http://www.imo.net/data/visual}

Rendtel J. and Arlt R., editors (2008). \emph{IMO Handbook
For Meteor Observers}. IMO, Potsdam.
}
\author{
Kristina Veljkovic
}

\seealso{
\code{\link{filter}}
}
\examples{
## select visual meteor data for the period from 5-15 August 2007

## rate data for the year 2007
data(rate07)
filter.datetime(rate07,year=2007,month.beg=8,day.beg=5,day.end=15)

## magnitude data for the year 2007
data(magn07)
filter.datetime(magn07,year=2007,month.beg=8,day.beg=5,day.end=15)

##select rate data for the period from 25 July 20h to 20 August, 3h
filter.datetime(rate07,year=2007,month.beg=7,month.end=8, day.beg=25,day.end=20,
time.beg=2000,time.end=300)

}


