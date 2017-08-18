\name{mag.distr}
\alias{mag.distr}
\title{
Summarized magnitude distribution
}
\description{Table and graphical representation of summarized magnitude distribution for a given magnitude dataset,
specified meteor shower and time period.
}
\usage{
mag.distr(data,date.start,date.end,shw)
}
\arguments{
  \item{data}{
 data frame consisting of visual meteor magnitude data.
}
  \item{date.start}{
character vector or factor specifying start date, given in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone).
}
  \item{date.end}{
character vector or factor specifying end date, given in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone). 
}
   \item{shw}{
character string consisting of three capital letters which represent meteor shower code.
} 
}
\details{Summarized magnitude distribution is formed by summing magnitude frequencies of all observing time intervals.
}
\value{Table and plot of summarized magnitude distribution consisting of histogram and boxplot.

The histogram cells are intervals of length 1, with midpoints at magnitudes -6 to +7.
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{pop.index}}
}
\examples{
## select 2015 magnitude data for observations of Orionids during the period 10-25th October
## and make graphics of magnitude distribution
mag.distr(magn2015,date.start="2015-10-10",date.end="2015-10-25", shw="ORI")
}




