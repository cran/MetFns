\name{filter.obsname}
\alias{filter.obsname}
\title{
Selection of visual meteor data by observer's first and last name
}
\description{
Selects data for a given visual meteor dataset and specified observer's first and last name.
}
\usage{
filter.obsname(data, name, fname)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{name}{
character string specifying observer's last name.
}
  \item{fname}{
character string specifying observer's first name.
}
}
\details{
List of observer's names can be found in the data frame \code{\link{vmdbpers}}.

\code{filter.obsname} can be used when one is not certain of 
IMO observer code (due to possible non-uniqueness of five letter combination).
}
\value{
\code{filter.obsname} returns data frame with the same number of columns as the argument \code{data},
containing values which correspond to the observer with specified first and last name.
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
Argument \code{data} has to consist of the column named "IMOcode".
}
\seealso{
\code{\link{filter.imocode}}, \code{\link{filter}}
}
\examples{
## select visual meteor data for the year 2004 corresponding to Ivana Marjanovic

## rate data for the year 2004
data(rate04)
filter.obsname(rate04,name="Marjanovic",fname="Ivana")

## magnitude data for the year 2004
data(magn04)
filter.obsname(magn04,name="Marjanovic",fname="Ivana")
}