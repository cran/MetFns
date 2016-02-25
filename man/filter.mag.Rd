\name{filter.mag}
\alias{filter.mag}
\title{
Selection of visual meteor data by limiting magnitude
}
\description{
Selects data for a given visual meteor dataset and specified limiting magnitude or interval of magnitudes.
}
\usage{
filter.mag(data,mag.low = 3,mag.up = 8)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{mag.low}{
numeric vector with value between 3 (default) and 8, specifying lower boundary of limiting magnitude.
}
  \item{mag.up}{
numeric vector with value between 3 and 8 (default), specifying upper boundary of limiting magnitude.
}
}
\value{
\code{filter.mag} returns data frame with the same number of columns as the argument \code{data},
containing observations with the limiting magnitudes between \code{mag.low} and \code{mag.up}.
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
Argument \code{data} has to consist of the column named "lmg".
}
\seealso{
\code{\link{filter}},\code{\link{filter.totcor}}
}
\examples{
## select visual meteor data for 13th August 2007, limiting magnitude between 5.5 and 6.5

## rate data for the year 2007
data(rate07)
rate1308<-filter.datetime(rate07,year=2007,month.beg=8,day.beg=13)
filter.mag(rate1308,mag.low=5.5,mag.up=6.5)

## magnitude data for the year 2007
data(magn07)
magn1308<-filter.datetime(magn07,year=2007,month.beg=8,day.beg=13)
filter.mag(magn1308,mag.low=5.5,mag.up=6.5)


## select visual meteor data for the 13th August 2007 and limiting magnitude above 5.5
filter.mag(rate1308,mag.low=5.5)
filter.mag(magn1308,mag.low=5.5)
}

