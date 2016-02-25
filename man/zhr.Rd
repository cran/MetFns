\name{zhr}
\alias{zhr}
\title{
Calculates zenithal hourly rate (ZHR)
}
\description{Calculates and plots average zenithal hourly rate of a meteor shower
for a given rate data, specified shower, period of days, population index, minimum and maximum bin sizes, number of meteors and ZHR
correction.
}
\usage{
zhr(data,year,month.beg,month.end=month.beg,day.beg,day.end=day.beg,
time.beg=0,time.end=2359,shw,r=NULL,Ralpha=NULL,Delta=NULL,k1=0.01,k2=1,num,C=1,
data2=NULL,add.plot=FALSE,xlim1=NULL,xlim2=NULL,xinc=NULL,ylim1=NULL,ylim2=NULL,
yinc=NULL)
}
\arguments{
  \item{data}{
  data frame consisting of visual meteor rate data.
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
   \item{r}{
 numeric vector specifying population index of a meteor shower.
}
   \item{Ralpha}{
 numeric vector with value between 0 and 360, specifying right ascension of the radiant, in degrees.
}
   \item{Delta}{
 numeric vector with value between -90 and +90, specifying declination of the radiant, in degrees.
}
   \item{k1}{
  numeric vector specifying minimum bin size, in degrees of solar longitude. By default, it is   equal to 0.01.
}
  \item{k2}{
  numeric vector specifying maximum bin size, in degrees of solar longitude. By default, it is   equal to 1.
}
\item{num}{
  numeric vector specifying total number of meteors per interval.
}
   \item{C}{
 numeric vector specifying value of ZHR correction. By default, it is equal to 1.
}

\item{data2}{
 data frame consisting of corresponding visual meteor magnitude data.
}
\item{add.plot}{
  logical vector. If TRUE, the population index is plotted.
}
\item{xlim1}{
  numeric vector specifying minimum value on x-axis.
}
\item{xlim2}{
  numeric vector specifying maximum value on x-axis.
}
\item{xinc}{
  numeric vector specifying increment between labels on x-axis.
}
\item{ylim1}{
  numeric vector specifying minimum value on y-axis.
}
\item{ylim2}{
  numeric vector specifying maximum value on y-axis.
}
\item{yinc}{
  numeric vector specifying increment between labels on y-axis.
} 
}
\details{Adaptive-bin size algorithm is used. It tries to maintain a used-defined optimum meteor
number per bin. For the case that not enough meteors are available, a maximum bin size is used.

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
\code{\link{radiant}} are used. If population index is not specified, it is calculated using the function \code{pop.index2} and then incorporated in the calculation of ZHR.

}
\value{
Data frame containing following numeric vectors
  \describe{
   \item{start}{factor  Calendar date and time in UTC of left bound of time interval}
   \item{stop}{factor  Calendar date and time in UTC of right bound of time interval}
   \item{sollong}{Solar longitude corresponding to the middle of time interval}
   \item{nINT}{Number of observing time intervals}
   \item{nSHW}{Number of observed meteors. String SHW is replaced with the code of meteor shower.}
   \item{ZHR}{Zenithal Hourly Rate}
   \item{st.err}{Standard error of ZHR}
   \item{density}{Spatial number density} 
   \item{dens.err}{Standard error of spatial number density}
}
If \code{add.plot} is TRUE, additionally xy plot of Zenithal Hourly Rate is made. Solar longitude is on x-axis and population index on y-axis. ZHR is represented with black filled circles and \emph{65\%} confidence intervals. Values of limits (minimum
and maximum values) on x and y axis (\code{xlim1,xlim2,ylim1,ylim2}), as well as increments between the axis labels 
(\code{xinc,yinc}) should be provided to function call. 
}
\references{
Rendtel J. and Arlt R., editors (2008). \emph{IMO Handbook
For Meteor Observers}. IMO, Potsdam.

Bias, P.V. (2011). A Note on Poisson inference and extrapolations under low raw data
and short interval observation conditions. \emph{WGN, Journal of the IMO},39:1, 14-19.
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{pop.index2}}
}
\examples{
##calculate ZHR for observations of 1997 Perseids, time period 10th August 19h to
##13th August 7h, radiant elevation higher than 20 degrees, total correction smaller than 5
##select 1997 Perseids, 28th July to 16th August, radiant elevation higher than 20 degrees
## data(rate97)
## rate<-filter(rate97,year=1997,month.beg=7,month.end=8,day.beg=28,day.end=16,
## shw="PER",h.low=20)
## zhr(rate,year=1997,month.beg=8,day.beg=10,day.end=13,time.beg=1900,time.end=700,
## shw="PER",r=2.2,num=300)

##make a graphic of ZHR
##x-axis limits: min(sollong)=138.252, max(sollong)=140.561
##y-axis limits: min(ZHR-st.err)=26.2, max(ZHR+st.err)=144.6

## zhr(rate,year=1997,month.beg=8,day.beg=10,day.end=13,time.beg=1900,time.end=700,
## shw="PER",r=2.2,num=300,add.plot=TRUE,xlim1=138.2,xlim2=140.6,xinc=0.1,
## ylim1=20,ylim2=160,yinc=20)

##calculate ZHR for the same data using population index calculated from 
##magnitude data
## data(magn97)
## magn<-filter(magn97,year=1997,month.beg=7,month.end=8,day.beg=28,day.end=16,
## shw="PER",h.low=20)
## zhr(rate,year=1997,month.beg=8,day.beg=10,day.end=13,time.beg=1900,time.end=700,
## shw="PER",num=300,data2=magn)

##make a graphic of ZHR
##x-axis limits: min(sollong)=138.252, max(sollong)=140.561
##y-axis limits: min(ZHR-st.err)=25.4, max(ZHR+1.96*st.err)=141.4
## zhr(rate,year=1997,month.beg=8,day.beg=10,day.end=13,time.beg=1900,time.end=700,
## shw="PER",r=2.2,num=300,data2=magn,add.plot=TRUE,xlim1=138.2,xlim2=140.6,xinc=0.1,
## ylim1=20,ylim2=160,yinc=20)


}




