\name{midint}
\alias{midint}
\title{
Calculation of the middle of time interval
}
\description{
Calculates the middle of observing time interval for a given visual meteor dataset.
}
\usage{
midint(data)
}
\arguments{
 \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
}
\value{
\code{midint} returns the middle of observing time interval, in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format, UTC timezone 
(object of \code{POSIXct} class).
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the columns named "Start.Date" and "End.Date".  These dates should be given in 
\code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone).
}
\seealso{
\code{\link{solar.long}}
}
\examples{
## calculate middle of time interval for rate and magnitude data, year 2015
midint(rate2015)

midint(magn2015)

}


