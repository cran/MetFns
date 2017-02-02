\name{mag.distr}
\alias{mag.distr}
\title{
Summarized magnitude distribution
}
\description{Table and graphical representation of summarized magnitude distribution for a given magnitude dataset,
specified meteor shower and time period.
}
\usage{
mag.distr(data,year,month.beg,month.end=month.beg,day.beg,day.end=day.beg,
time.beg=0,time.end=2359,shw)
}
\arguments{
  \item{data}{
 data frame consisting of visual meteor magnitude data.
}
  \item{year}{
numeric vector of length 4 specifying year.
}
   \item{month.beg}{
numeric vector specifying the beginning month.
}
  \item{month.end}{
numeric vector specifying the ending month. By default, \code{month.end} is set to be equal to \code{month.beg}.
}
  \item{day.beg}{
numeric vector specifying the beginning day.
}
  \item{day.end}{
numeric vector specifying the ending day. By default, \code{day.end} is set to be equal to \code{day.beg}.
}
  \item{time.beg}{
numeric vector (0-2359) specifying lower boundary of time in hours and minutes, corresponding to \code{day.beg}. By default, \code{time.beg} is set to be equal to 0.
}
  \item{time.end}{
numeric vector(0-2359) specifying upper boundary of time in hours and minutes, corresponding to \code{day.end}. By default, \code{time.end} is set to be equal to 2359.
}
   \item{shw}{
character string consisting of three capital letters which represent meteor shower code.
} 
}
\details{Summarized magnitude distribution is formed by summing frequencies of all observers for each magnitude value.
}
\value{Table and plot of summarized magnitude distribution consisting of histogram and boxplot.

The histogram cells are intervals of length 1, with midpoints at magnitude values.
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{pop.index}}
}
\examples{
## select data for observations of Perseids, period 12-14th August  2007 
## and make graphics of magnitude distribution
data(magn07)
magnPer<-filter(magn07,shw="PER", year=2007, month.beg=8, day.beg=12, day.end=14)
mag.distr(magnPer,year=2007, month.beg=8, day.beg=12, day.end=14, shw="PER")
}




