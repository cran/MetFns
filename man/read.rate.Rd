\name{read.rate}
\alias{read.rate}
\title{
Read rate data
}
\description{Reads rate data from IMO site or saved file.
}
\usage{
read.rate(data)
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
\code{\link{read.magn}}
}
\examples{
## read rate data for the year 2011 from IMO site
read.rate("http://www.imo.net/files/data/vmdb/RATE_11.TXT")
}