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
List of meteor shower codes can be found in the dataframe \code{\link{shw_list}}. Code "SPO" represents sporadic showers.
}
\value{
\code{filter.shw} returns data frame containing observations which correspond to specified meteor shower.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the column named "Shower". 
}
\seealso{
\code{\link{filter}}
}
\examples{
## select rate and magnitude data for 2015 Perseids
filter.shw(rate2015,shw="PER")
filter.shw(magn2015,shw="PER")
} 