\name{zhr}
\alias{zhr}
\title{
Calculates zenithal hourly rate (ZHR)
}
\description{Calculates average zenithal hourly rate of a meteor shower
for a given rate data, specified shower, period of days, population index, minimum and maximum bin sizes, number of meteors and ZHR
correction.
}
\usage{
zhr(data,date.start,date.end,shw,r=NULL,kmin=0.01,kmax=1,num,c.zhr=0.5,rdata=NULL)
}
\arguments{
  \item{data}{
  data frame consisting of visual meteor rate data.
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
   \item{r}{
 numeric vector specifying population index of a meteor shower.
}
   \item{kmin}{
  numeric vector specifying minimum bin size, in degrees of solar longitude. By default, it is equal to 0.01. Minimum accepted value is 0.001.
}
  \item{kmax}{
  numeric vector specifying maximum bin size, in degrees of solar longitude. By default, it is equal to 1. Maximum accepted values is 5.
}
\item{num}{
  numeric vector specifying total number of meteors per interval. Minimum accepted value is 1.
}
   \item{c.zhr}{
 numeric vector specifying value of ZHR correction. By default, it is equal to 0.5. Accepted values are between 0 and 1.
}

\item{rdata}{
 data frame consisting of population index values calculated on corresponding visual meteor magnitude data.
}
}
\details{
Optimal bin size algorithm is used. It searches for an optimal bin size between minimum bin size \code{kmin} and maximum bin size \code{kmax} with total number of meteors per bin \code{num}. If there are not enough meteors, a maximum bin size is used. For further calculations, only observing intervals with lengths smaller or equal to optimal bin are used.

Average zenithal hourly rate is calculated by the formula 

\deqn{ZHR=\frac{c+\sum_i N_i}{\left(\sum_i \frac{T_{eff,i}}{C_i}\right)}, i=1,2,...,k}{ZHR=(c+sum_i N_i)/(sum_i T_{eff,i}/C_i), i=1,2,...,k}

where \eqn{k} is the number of observing periods, \eqn{N_i} - the raw number of meteors seen by each observer in
observing period \eqn{i}, \eqn{T_{eff,i}} - the effective time or amount of time an observer actually scans the sky for meteors
during observing period \eqn{i}, and \eqn{C_i} - total correction factor that accounts for all the imperfections in the observing
period \eqn{i} such as clouds, low radiant, low limiting magnitude.

Total correction factor is equal to 

\deqn{C_i=\frac{r^{(6.5-lmg_i)}F_i}{\sin(h_i)}}{C_i=r^(6.5-lmg_i)F_i/sin(h_i)},

where \eqn{r} is population index, \eqn{lmg_i} limiting magnitude, \eqn{F_i} correction  factor for field-of-view obstruction,
\eqn{h_i} radiant elevation for each observer in observing period \eqn{i}.

In the numerator, \eqn{c} is included to correct for the asymmetric high and low end possibilities in
a Poisson distribution (distribution of the number of observed meteors).

Standard error of the average zenithal rate is calculated by the formula

\deqn{st.error=\frac{\sqrt{c+\sum_i N_i}}{\sum_i \frac{T_{eff,i}}{C_i}}, i=1,2,...,k}{st.error=sqrt(c+sum_i N_i)/(sum_i T_{eff,i}/C_i), i=1,2,...,k}.


The spatial number density of meteoroids producing meteors of magnitude at least 6.5 is (per \eqn{10^9km^3})

\deqn{\rho=\frac{(10.65r-12.15)ZHR}{3600 178700r^(-1.82)V}}{rho=(10.65r-12.15)ZHR/(3600* 178700r^(-1.82)V)},

where \eqn{V} is stream's geocentric velocity. 

Standard error of spatial number density is approximated with

\deqn{\sigma_{\rho}=\frac{\sigma \rho}{ZHR}}{sigma_{rho}=sigma* rho/ZHR}.

If right ascension and declination of shower radiant are not specified, the values from the data frame
\code{\link{radiant}} are used. If population index is not specified, it should be calculated using functions \code{pop.index} or \code{pop.index2} and then incorporated in the calculation of ZHR (through dataframe \code{rdata}).
}
\value{
Data frame containing following vectors
  \describe{
   \item{sollong}{numeric Mean of observers' solar longitudes weighted by \eqn{\frac{T_{eff,obs}}{C_{obs}}}{T_{eff,obs}/C_{obs}}, observers effective time
   divided by total correction factor}
   \item{date}{POSIXct  Calendar date and time in UTC corresponding to \code{sollong}}
   \item{nINT}{numeric Number of observing time intervals}
   \item{nSHW}{numeric Number of observed meteors. String SHW is replaced with the code of meteor shower.}
   \item{ZHR}{numeric Zenithal Hourly Rate}
   \item{st.error}{numeric Standard error of ZHR}
   \item{density}{numeric Spatial number density} 
   \item{dens.error}{numeric Standard error of spatial number density}
}
}
\references{
Rendtel J. and Arlt R., editors (2014). \emph{Handbook
For Meteor Observers}. IMO, Potsdam.

Bias, P.V. (2011). A Note on Poisson inference and extrapolations under low raw data
and short interval observation conditions. \emph{WGN, Journal of the IMO},39:1, 14-19.
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{zhr.plot}},\code{\link{pop.index2}}
}
\examples{
## calculate ZHR for observations of 2015 Orionids, time period around maximum,
## 2th to 25th October, min bin size 0.1 degree, max bin size 1 degree, 
## number of meteors equals 100, population index calculated from magnitude data
## First select rate data for Orionids activity from 21th to 25th October, 
## limiting magnitudes above 5, radiant elevation above 20 degrees, percentage of clouds below 20
ori2015<-filter(rate2015,date.start="2015-10-21",date.end="2015-10-25",shw="ORI",
P.up=20,mag.low=5,mag.up=8,h.low=20)
ori2015mag<-filter(magn2015,date.start="2015-10-21",date.end="2015-10-25",shw="ORI",
P.up=20,mag.low=5,h.low=20)
oripop<-pop.index2(ori2015mag,date.start="2015-10-21",date.end="2015-10-25",
shw="ORI",kmin=0.1,kmax=1,num=100)
zhr(ori2015,date.start="2015-10-21",date.end="2015-10-25",shw="ORI",
kmin=0.1,kmax=1,num=100,rdata=oripop)
}




