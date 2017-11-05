\name{filter.mag}
\alias{filter.mag}
\title{
Selection of visual meteor data by limiting magnitude
}
\description{
Selects data for a given visual meteor dataset and specified limiting magnitude or interval of magnitudes.
}
\usage{
filter.mag(data,mag.low=1,mag.up = 8)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor rate data.
}
  \item{mag.low}{
numeric vector with value between 1 (default) and 8, specifying lower boundary of limiting magnitude.
}
  \item{mag.up}{
numeric vector with value between 1 and 8 (default), specifying upper boundary of limiting magnitude.
}
}
\value{
\code{filter.mag} returns data frame with the same number of columns as the argument \code{data},
containing observations with the limiting magnitudes between \code{mag.low} and \code{mag.up}.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the column named "Lmg".
}
\seealso{
\code{\link{filter}},\code{\link{filter.totcor}}
}
\examples{
## select visual meteor rate data for 13th August 2015, limiting magnitude between 5.5 and 6.5
rate_13.08<-filter.date(rate2015,date.start="2015-08-13",date.end="2015-08-13")
filter.mag(rate_13.08,mag.low=5.5,mag.up=6.5)

## select visual meteor rate data for the 13th August 2015 and limiting magnitude above 6
filter.mag(rate_13.08,mag.low=6)
}

