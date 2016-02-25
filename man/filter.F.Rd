\name{filter.F}
\alias{filter.F}
\title{
Selection of visual meteor data by correction factor for clouds
}
\description{
Selects data for a given visual meteor rate dataset and specified correction factor or interval of correction 
factors for clouds.
}
\usage{
filter.F(data,F.low = 1,F.up = 3)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor rate data.
}
  \item{F.low}{
numeric vector with value between 1 (default) and 3, specifying lower boundary of correction factor for clouds. 
}
  \item{F.up}{
numeric vector with value between 1 and 3 (default), specifying upper boundary of correction factor for clouds.
}
}
\details{
Correction factor for clouds is calculated by the formula
\emph{F=1/(1-p)}, where \emph{p} is a fraction (\emph{0<p<1}) of clouds covering observing field of view.
}
\value{
\code{filter.F} returns data frame with the same number of columns as the argument \code{data},
containing observations with the correction factors for clouds between \code{F.low} and \code{F.up}.
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
Argument \code{data} has to consist of the column named "F".
}
\seealso{
\code{\link{filter}},\code{\link{filter.totcor}}
}
\examples{
## select visual meteor data for the period between 15-30th October 2005 and 
## correction factor for clouds below 1.25 (which corresponds to 20% of clouds)

## rate data for the year 2005
data(rate05)
rateOct<-filter.datetime(rate05,year=2005,month.beg=10,day.beg=15,day.end=30)
filter.F(rateOct,F.up=1.25)
}