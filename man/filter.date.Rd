\name{filter.date}
\alias{filter.date}
\title{
Selection of visual meteor data by date(s)
}
\description{
Selects data for a given visual meteor dataset and specified year, month and day (or days).
}
\usage{
filter.date(data, year, month, day.beg, day.end = day.beg)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{year}{
numeric vector of length 4 specifying year.
}
  \item{month}{
numeric vector specifying month of the year.
}
  \item{day.beg}{
numeric vector specifying the beginning day.
}
  \item{day.end}{
numeric vector specifying the ending day. By default, \code{day.end} is set to be equal to \code{day.beg}.
}
}
\details{
Day given in meteor datasets corresponds to the beginning of the observing time period. In selection of 
the data, day corresponding to the middle of the observing time period is used.

If argument \code{day.end} is not provided, the function \code{filter.date} selects data for a given date, 
otherwise it selects data for a period of days, bounded by \code{day.beg} and \code{day.end}.
}
\value{
\code{filter.date} returns data frame with the same number of columns as the argument \code{data},
containing observations which correspond to the specified date or period of days.
}
\references{
\url{http://www.imo.net/data/visual}

Rendtel J. and Arlt R., editors (2008). \emph{IMO Handbook
For Meteor Observers}. IMO, Potsdam.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the columns named "year", "month" and "day".
}
\seealso{
\code{\link{filter}}
}
\examples{
## select visual meteor data for the period from 5-15 August 2007

## rate data for the year 2007
data(rate07)
filter.date(rate07,year=2007,month=8,day.beg=5,day.end=15)

## magnitude data for the year 2007
data(magn07)
filter.date(magn07,year=2007,month=8,day.beg=5,day.end=15)
}


