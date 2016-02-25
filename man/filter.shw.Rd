\name{filter.shw}
\alias{filter.shw}
\title{
Selection of visual meteor data by shower code
}
\description{
Selects data for a given visual meteor dataset and specified shower code.
}
\usage{
filter.shw(data,shw)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{shw}{
character string consisting of three capital letters which represent meteor shower code. 
}
}
\details{
List of meteor shower codes can be found in the dataframe \code{\link{shw_list}}. 
}
\value{
\code{filter.shw} returns data frame containing observations which correspond to specified meteor shower.
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
Argument \code{data} has to consist of the column named "SPO" in rate data frames (placed before the 
columns for showers) and columns "zero" and "Shw" in magnitude data frames.
}
\seealso{
\code{\link{filter}}
}
\examples{
## select visual meteor data for Perseids 2000 

## rate data for the year 2000
data(rate00)
filter.shw(rate00,shw="PER")

## magnitude data for the year 2000
data(magn00)
filter.shw(magn00,shw="PER")
} 