\name{filter.date}
\alias{filter.date}
\title{
Selection of visual meteor data by date(s)
}
\description{
Selects data for a given visual meteor dataset and specified dates.
}
\usage{
filter.date(data,date.start,date.end)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{date.start}{
character vector or factor specifying start date, given in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone).
}
  \item{date.end}{
character vector or factor specifying end date, given in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone). 
}
}
\details{
The function \code{filter.date} selects data for a time period, bounded by \code{date.start} and \code{date.end}.  In selection of the data, date corresponding to the middle of the observing time period is used.}
\value{
\code{filter.date} returns data frame with the same number of columns as the argument \code{data},
containing observations which correspond to the specified dates.
}
\author{
Kristina Veljkovic
}
\note{
Hour, seconds and minutes do not have to be provided in function call.

Argument \code{data} has to consist of the columns named "Start.Date" and "End.Date".
}
\seealso{
\code{\link{filter}} 
}
\examples{
## select rate and magnitude data for the period from 10-25th October 2015
filter.date(rate2015,date.start="2015-10-10",date.end="2015-10-25")
filter.date(magn2015,date.start="2015-10-10",date.end="2015-10-25")

}


