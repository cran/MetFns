\name{filter.country}
\alias{filter.country}
\title{
Selection of visual meteor data by country
}
\description{
Selects data for a given visual meteor dataset and specified country.
}
\usage{
filter.country(data,country)
}
\arguments{
  \item{data}{
 data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{country}{
character string specifying the name of the observing country.
}
}
\value{
\code{filter.country} returns data frame with the same number of columns as argument \code{data},
containing observations corresponding to the specified country.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the column named "Country".
}
\seealso{
\code{\link{filter}}
}
\examples{
## select 2015 rate and magnitude data from Serbia
filter.country(rate2015,country="Serbia")

filter.country(magn2015,country="Serbia")
}