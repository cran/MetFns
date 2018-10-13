\name{pop.index}
\alias{pop.index}
\title{
Calculation of population index
}
\description{Calculates population index of a meteor shower
for a given magnitude data, specified time period, magnitude values and bin size.
}
\usage{
pop.index(data,date.start,date.end,shw,mag.range=-6:7,k,gamma=1)
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
   \item{mag.range}{
  numeric vector specifying range of magnitudes. It should consist of at least 5 magnitude classes.
}
\item{k}{
  numeric vector specifying bin size in degrees of solar longitude. Minimum accepted value is 0.001 and maximum accepted value is 5.
}
\item{gamma}{
  numeric vector specifying zenith exponent. It takes value between 1 (default) and 2.
}

}
\details{Probabilities of perception are incorporated in magnitude distributions for each observing interval.
Cummulative summarized magnitude distribution \eqn{\Phi(m)} is formed by summing cummulative frequencies of all observers for each magnitude class \eqn{m}.

Using the relationship for population index \eqn{r=\frac{\Phi(m+1)}{\Phi(m)}}{r=\Phi(m+1)/\Phi(m)} and substitutiong \eqn{0,1,...m} 
magnitudes, 
equation \eqn{\Phi(m)=\Phi(0)r^m} (or \eqn{ln(\Phi(m))=ln(\Phi(0))+r ln(m)} in logarithmic form) can be written.
Then, population index \eqn{r} is calculated by the method of least squares, for chosen range of magnitude values.

Standard error of population index is approximated with

\deqn{\sigma_r= r \sqrt{\exp{\left(\frac{\sum_j e_j^2}{(n-2)\sum_j m_j^2}\right)}\left(\exp{\left(\frac{\sum_j e_j^2}{(n-2)\sum_j m_j^2}\right)}-1\right)}}{sigma_r= r sqrt(exp (sum_j e_j^2/((n-2)sum_j m_j^2)) *(exp (sum_j e_j^2/((n-2)sum_j m_j^2))-1))}

where \eqn{n} is number of magnitude values, \eqn{m_j} magnitude values, \eqn{e_j} regression residuals, 
\eqn{j=1,2,...n}.

}
\value{
Data frame containing following vectors
\describe{
   \item{sollong}{numeric Mean of observers' solar longitudes weighted by \eqn{\frac{N_{obs}}{C_{obs}}}{N_{obs}/C_{obs}}, observers' numbers of meteors divided by total correction factor}
   \item{date}{POSIXct  Calendar date and time in UTC corresponding to \code{sollong}}
   \item{mag}{factor  Range of magnitude values}
   \item{nINT}{numeric Number of observing time intervals}
   \item{nSHW}{numeric Number of observed meteors belonging to the shower}
   \item{pop.index}{numeric Population index}
   \item{r.error}{numeric Standard error of population index}
}
}      
\references{

Koschack R. and Rendtel J. (1990). Determination of
spatial number density and mass index from visual
meteor observations (1). \emph{WGN, Journal of the IMO}, 18(2), 44 - 58.

Koschack R. and Rendtel J. (1990). Determination of
spatial number density and mass index from visual
meteor observations (2). \emph{WGN, Journal of the IMO}, 18(4), 119 - 140.

Rendtel J. and Arlt R., editors (2008). \emph{IMO Handbook
For Meteor Observers}. IMO, Potsdam.
}
\author{
Kristina Veljkovic
}
\note{
The interval for regression is chosen such that: there is at least 3 meteors per magnitude class,
the magnitude classes m<=5 are included and there are at least 5 magnitude classes available.  All these conditions are fulfilled for
the range of magnitude values printed in results. 

For calculation of population index, only observing intervals with lengths smaller or equal to bin size \code{k} are used. 
}
\seealso{
\code{\link{mag.distr}},\code{\link{pop.index2}},\code{\link{zhr}}
}
\examples{
##calculate population index for observations of 2015 Orionids, 
## time period 21th to 25th October, bin size 1 degree

## First select magnitude data for Orionids activity from 21th to 25th October, 
## limiting magnitudes above 5, radiant elevation above 20 degrees,
## percentage of clouds below 20
magnOri<-filter(magn2015,date.start="2015-10-21",date.end="2015-10-25",
shw="ORI",P.up=20,mag.low=5,h.low=20)
pop.index(magnOri,date.start="2015-10-21",date.end="2015-10-25",shw="ORI",k=1)

}








