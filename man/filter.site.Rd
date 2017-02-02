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
\details{
List of site names can be found in the dataframe \code{\link{vmdbsite}}.
}
\value{
\code{filter.site} returns data frame with the same number of columns as the argument \code{data},
containing observations which correspond to specified observing site.
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
\code{\link{filter}},\code{\link{filter.country}}
}
\examples{
## select visual meteor data for the year 2009 from Debelo brdo site

## rate data for the year 2009
data(rate09)
filter.site(rate09,site="Debelo brdo")

## magnitude data for the year 2009
data(magn09)
filter.site(magn09,site="Debelo brdo")
}