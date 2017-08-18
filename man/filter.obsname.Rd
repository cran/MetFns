\name{filter.obsname}
\alias{filter.obsname}
\title{
Selection of visual meteor data by observer's first and last name
}
\description{
Selects data for a given visual meteor dataset and specified observer's first and last name.
}
\usage{
filter.obsname(data,fname,lname)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{fname}{
character string specifying observer's first name.
}
  \item{lname}{
character string specifying observer's last name.
}
}
\value{
\code{filter.obsname} returns data frame with the same number of columns as the argument \code{data},
containing values which correspond to the observer with specified first and last name.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the columns named "First.Name" and "Last.Name".
}
\seealso{
\code{\link{filter}}
}
\examples{
## select 2015 rate and magnitude data of observer Javor Kac
filter.obsname(rate2015,fname="Javor",lname="Kac")
filter.obsname(magn2015,fname="Javor",lname="Kac")
}