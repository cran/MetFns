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
character string specifying the name of the country.
}
}
\details{
List of the countries can be found in the data frame \code{\link{vmdbsite}}. Data selection is performed using
 \code{\link{filter.site}} which filters data by codes of all sites belonging to the specified country.
}
\value{
\code{filter.country} returns data frame with the same number of columns as argument \code{data},
containing observations corresponding to the specified country.
}
\references{
Rendtel J. and Arlt R., editors (2008). \emph{IMO Handbook
For Meteor Observers}. IMO, Potsdam.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the column named "sitecode".
}
\seealso{
\code{\link{filter}}
}
\examples{
## select visual meteor data for the year 2009 from Serbia

## rate data for the year 2009
data(rate09)
filter.country(rate09,country="Serbia")

## magnitude data for the year 2009
data(magn09)
filter.country(magn09,country="Serbia")
}