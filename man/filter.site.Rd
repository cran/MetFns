\name{filter.site}
\alias{filter.site}
\title{
Selection of visual meteor data by observing site
}
\description{
Selects data for a given visual meteor dataset and specified observing site.
}
\usage{
filter.site(data,site)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{site}{
character string specifying name of the observing site.
}
}
\value{
\code{filter.site} returns data frame with the same number of columns as the argument \code{data},
containing observations which correspond to specified observing site.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the column named "City".
}
\seealso{
\code{\link{filter}},\code{\link{filter.country}}
}
\examples{
## select 2015 rate and magnitude data from Debelo brdo site
filter.site(rate2015,site="Debelo brdo")
filter.site(magn2015,site="Debelo brdo")
}