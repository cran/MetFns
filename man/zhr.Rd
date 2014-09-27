\name{zhr}
\alias{zhr}
\title{
Calculates zenithal hourly rate (ZHR)
}
\description{Calculation of average zenithal hourly rate of a meteor shower
for a given rate data, specified shower, period of days, population index, length of time interval and ZHR correction.
}
\usage{
zhr(data,year, month, day.beg,day.end,shw, r=NULL, Ralpha=NULL, Delta=NULL, k,c=1)
}
\arguments{
  \item{data}{
  data frame consisting of visual meteor rate data.
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
   \item{r}{
 numeric vector specifying population index of a meteor shower.
}
   \item{Ralpha}{
 numeric vector with value between 0 and 360, specifying right ascension of the radiant, in degrees.
}
   \item{Delta}{
 numeric vector with value between -90 and +90, specifying declination of the radiant, in degrees.
}
   \item{k}{
 numeric vector specifying length of time interval in hours.
}
   \item{c}{
 numeric vector specifying value of ZHR correction. By default, it is equal to 1.
}
}
\details{
Average zenithal hourly rate is calculated by the formula 

\emph{ZHR=(c+sum_i n_i)/(sum_i T_{eff,i}/C_i), i=1,2,...,k}

where \emph{k} is the number of observing periods, \emph{n_i} - the raw number of meteors seen by each observer in
observing period \emph{i}, \emph{T_{eff,i}} - the effective time or amount of time an observer actually scans the sky for meteors
during observing period \emph{i}, and \emph{C_i} - a correction factor that accounts for all the imperfections in the observing
period \emph{i} such as clouds, low radiant, low limiting magnitude.

Correction factor is equal to 

\emph{C_i=r^(6.5-lmg_i)F_i/sin(h_i)},

where \emph{r} is population index, \emph{lmg_i} limiting magnitude, \emph{F_i} correction  factor for clouds,
\emph{h_i} radiant elevation for each observer in observing period \emph{i}.

In the numerator, \emph{c} is included to correct for the asymmetric high and low end possibilities in
a Poisson distribution (distribution of number of observed meteors).

Standard error of the average zenithal rate is calculated by the formula

\emph{sigma=ZHR/sqrt(c+sum_i n_i)}.


The spatial number density of meteoroids producing meteors of magnitude at least 6.5 is (per \emph{10^9km^3})

\emph{rho=(10.65r-12.15)ZHR/(3600 178700r^(-1.82)V)},

where \emph{V} is stream's geocentric velocity. 

Standard error of spatial number density is approximated with

\emph{sigma_{rho}=sigma rho/ZHR}.

If right ascension and declination of shower radiant are not specified, the values from the data frame
\code{\link{radiant}} are used.

Day is divided in subintervals of \emph{k} hours. For example, if \emph{k=12}, subintervals are [0,12) and [12,24).
Zenithal hourly rate is calculated for each subinterval in the following manner: If middle of observing time 
period belongs to the subinterval, corresponding data values are used in calculation of ZHR.  
}
\value{
Data frame containing following numeric vectors
  \describe{
   \item{day}{Day of the month}
   \item{month}{Month of the year}
   \item{year}{Year}
   \item{start}{Left bound of time subinterval}
   \item{stop}{Right bound of time subinterval}
   \item{sollong}{Solar longitude corresponding to the middle of time subinterval}
   \item{nINT}{Number of observing time intervals}
   \item{nSHW}{Number of observed meteors. String SHW is replaced with the code of meteor shower.}
   \item{ZHR}{Zenithal Hourly Rate}
   \item{st.err}{Standard error of ZHR}
   \item{density}{Spatial number density} 
   \item{dens.err}{Standard error of spatial number density}
}
}
\references{
Rendtel J. and Arlt R., editors (2008). \emph{IMO Handbook
For Meteor Observers}. IMO, Potsdam.

Koschack R. and Rendtel J. (1990a). Determination of
spatial number density and mass index from visual
meteor observations (1). \emph{WGN, Journal of the IMO}, 18(2), 44 - 58.

Bias, P.V. (2011). A Note on Poisson inference and extrapolations under low raw data
and short interval observation conditions. \emph{WGN, Journal of the IMO},39:1, 14-19.
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{zhr.graph}},\code{\link{pop.index}}
}
\examples{
## select visual meteor data for observation of Orionids,period 20-24th October 2006, 
## 12hrs time intervals, and calculate ZHR 
data(rate06)
rateOri<-filter(rate06,shw="ORI",year=2006, month=10, day.beg=20,day.end=24)
zhr(rateOri,year=2006,month=10,day.beg=20,day.end=24,shw="ORI",r=2.5,k=12)
}




