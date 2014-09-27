\name{read.magn}
\alias{read.magn}
\title{
Read magnitude data
}
\description{
Reads magnitude data from IMO site or saved file.
}
\usage{
read.magn(data)
}
\arguments{
  \item{data}{
the name of the file or connection to read from.
}
}
\details{
Argument \code{data} represents path to the file to be opened or a complete URL.
}
\value{
A data frame containing a representation of the data in the file.
}
\references{
\url{http://www.imo.net/data/visual}
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{read.rate}}
}
\examples{
## read magnitude data for the year 2011 from IMO site
read.magn("http://www.imo.net/files/data/vmdb/MAGN_11.TXT")
}