\name{filter.P}
\alias{filter.P}
\title{
Selection of visual meteor data by percentage of field-of-view obstruction
}
\description{
Selects data for a given visual meteor rate dataset and specified percentage or interval of percentages
of field-of-view obstruction.
}
\usage{
filter.P(data,P.low = 0,P.up = 20)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor rate data.
}
  \item{P.low}{
numeric vector with value between 0 (default) and 100, specifying lower boundary of percentage of field-of-view obstruction. 
}
  \item{P.up}{
numeric vector with value between 0 and 100, specifying upper boundary of percentage of field-of-view obstruction. Default value is 20.
}
}
\value{
\code{filter.P} returns data frame with the same number of columns as the argument \code{data},
containing observations with the percentage of field-of-view obstruction between \code{P.low} and \code{P.up}.
}
\references{
Rendtel J. and Arlt R., editors (2014). \emph{Handbook
For Meteor Observers}. IMO, Potsdam.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the column named "P".
}
\seealso{
\code{\link{filter}},\code{\link{filter.totcor}}
}
\examples{
## select visual meteor rate data for the period between 10-25th October 2015 and 
## percentage of field-of-view obstruction below 20%
rateOct<-filter.date(rate2015,date.start="2015-10-10",date.end="2015-10-25")
filter.P(rateOct)
}