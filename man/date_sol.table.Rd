\name{date_sol.table}
\alias{date_sol.table}
\title{
Calculation of table of solar longitudes
}
\description{
Calculates table of solar longitudes for given dates and step.
}
\usage{
date_sol.table(date.beg,date.end,step)
}
\arguments{
 \item{date.beg}{
character vector specifying beginning date, given in \code{"YYYY-mm-dd"} format (UTC timezone).
}
\item{date.end}{
character vector specifying ending date, given in \code{"YYYY-mm-dd"} format (UTC timezone).
}
\item{step}{
numeric vector specifying step in minutes. Possible values are 5,10,15,20,30,60,120 minutes.
}
}
\details{
The function \code{date_sol.table} calculates table of solar longitudes using function \code{solar.long} with days as row names and time in hours and minutes
as column names. Solar longitude values correspond to beginning date at midnight to ending date at midnight-step.}
\value{
\code{date_sol.table} returns dataframe of solar longitudes rounded to three decimals.
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{solar.long}}
}
\examples{
## calculate table of solar longitudes for 10-14 December 2015, step 120 minutes
date_sol.table("2015-12-10","2015-12-14",120)

}