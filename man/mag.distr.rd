\name{mag.distr}
\alias{mag.distr}
\title{
Graphics of magnitude distribution
}
\description{Graphical representation of magnitude distribution for a given magnitude dataset,
specified meteor shower and period of days.
}
\usage{
mag.distr(data,year, month, day.beg, day.end=day.beg, shw)
}
\arguments{
  \item{data}{
 data frame consisting of visual meteor magnitude data.
}
  \item{year}{
numeric vector of length 4 specifying year.
}
   \item{month}{
numeric vector specifying month of the year.
}
   \item{day.beg}{
numeric vector specifying beginning day.
}
   \item{day.end}{
numeric vector specifying ending day.
}
   \item{shw}{
character string consisting of three capital letters which represent meteor shower code.
} 
}
\details{Summarized magnitude distribution is formed by summing frequencies of all observers for each magnitude value.
}
\value{Plot of summarized magnitude distribution consisting of histogram and boxplot.

The histogram cells are intervals of the magnitudes of the form [a, b).
}
\references{
\url{http://www.imo.net/data/visual}
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the columns named "m6" and "p7". 
}
\seealso{
\code{\link{pop.index}}
}
\examples{
## select data for observations of Perseids, period 12-14th August  2007 
## and make graphics of magnitude distribution
data(magn07)
magnPer<-filter(magn07,shw="PER", year=2007, month=8, day.beg=12, day.end=14)
mag.distr(magnPer,year=2007, month=8, day.beg=12, day.end=14, shw="PER")
}




