\name{pop.index2}
\alias{pop.index2}
\title{
Calculation of population index based on average distance from the limiting magnitude
}
\description{Calculates and plots population index of a meteor shower
for a given magnitude data, specified time period, minimum and maximum bin size, and number of meteors.
}
\usage{
pop.index2(data,year,month.beg,month.end=month.beg,day.beg,day.end=day.beg,
time.beg=0,time.end=2359,shw,k1=0.01,k2=1,num,add.plot=FALSE,xlim1=NULL,
xlim2=NULL,xinc=NULL,ylim1=NULL,ylim2=NULL,yinc=NULL) 
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
numeric vector (0-2359) specifying upper boundary of time in hours and minutes, corresponding to \code{day.end}. By default, \code{time.end} is set to be equal to 2359.
}
   \item{shw}{
 character string consisting of three capital letters which represent meteor shower code.
}
   \item{k1}{
  numeric vector specifying minimum bin size, in degrees of solar longitude. By default, it is equal to 0.01.
}
  \item{k2}{
  numeric vector specifying maximum bin size, in degrees of solar longitude. By default, it is equal to 1.
}
\item{num}{
  numeric vector specifying total number of meteors per interval.
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

Average distance from the limiting magnitude, as difference between the limiting magnitude and average meteor magnitude, is calculated for each observing time interval. Final average distance from the limiting magnitude is calculated as a weighted average of all individual average distances, where numbers of meteors in each observing interval represent weights. Conversion of average distance from limiting magnitude to population index is done using natural spline interpolation \code{spline} of table values 
\code{\link{popind}}. 
Error margins of population index are calculated using bivariate Krige interpolation \code{krigeInterp} of table values 
\code{\link{popind.err}}  
}
\value{
Data frame containing following vectors
\describe{
   \item{start}{factor  Calendar date and time in UTC of left bound of time interval}
   \item{stop}{factor  Calendar date and time in UTC of right bound of time interval}
   \item{sollong}{numeric Solar longitude corresponding to the middle of time interval}
   \item{nINT}{numeric Number of observing time intervals}
   \item{nSHW}{numeric Number of observed meteors belonging to the shower}
   \item{pop.index}{numeric Population index}
   \item{r.error}{numeric Error margins of population index}
}

If \code{add.plot} is TRUE, additionally xy plot of population index is made. Solar longitude is on x-axis and population index on y-axis. Population index is represented with black filled circles and \emph{68\%} confidence intervals. Values of limits (minimum
and maximum values) on x and y axis (\code{xlim1,xlim2,ylim1,ylim2}), as well as increments between the axis labels 
(\code{xinc,yinc}) should be provided to function call. 
}
\references{
Arlt, R. (2003). Bulletin 19 of the International Leonid Watch: Population index study of the 2002 Leonid meteors. \emph{WGN, Journal of the IMO},31:3, 77-87.
}
\author{
Kristina Veljkovic
}
\seealso{
\code{\link{pop.index}},\code{\link{zhr}}
}
\examples{
## calculate population index for observations of 1997 Perseids, time period 
## 27th July to 16th August, radiant elevation higher than 20 degrees,
## total correction factor smaller than 5, bin sizes 1 degree
## First select magnitude data for Perseids activity - time period 15th July to 24th August, 
## radiant elevation higher than 20 degrees, total correction factor smaller than 5
## data(magn97)
## magn<-filter(magn97,year=1997,month.beg=7,month.end=8,day.beg=15,day.end=24,
## shw="PER",h.low=20)
## pop.index2(magn,year=1997,month.beg=7,month.end=8,day.beg=27,day.end=16,shw="PER",
## k1=1,num=300)

## make graphic of population index
## x-axis limits: min(sollong)=124.549, max(sollong)=144.111
## y-axis limits: min(pop.index-sigma.r)=1.91,max(pop.index+sigma.r)=3.07
## pop.index2(magn,year=1997,month.beg=7,month.end=8,day.beg=27,day.end=16,shw="PER",
## k1=1,num=300,add.plot=TRUE,xlim1=124,xlim2=145,xinc=1,ylim1=1.9,ylim2=3.1,yinc=0.1)




## calculate population index around maximum activity of 1997 Perseids, time period 
## 10th August 19h to 14th August 7h, radiant elevation higher than 20 degrees, 
## total correction factor smaller than 5
## pop.index2(magn,year=1997,month.beg=8,day.beg=10,day.end=14,time.beg=1900,
## time.end=700,shw="PER",num=300)


## make graphic of population index around max activity of 1997 Perseids
## x-axis limits: min(sollong)=138.272, max(sollong)=141.397
## y-axis limits: min(pop.index-sigma.r)=1.59,max(pop.index+sigma.r)=2.79
## pop.index2(magn,year=1997,month.beg=8,day.beg=10,day.end=14,time.beg=1900,
## time.end=700,shw="PER",num=300,add.plot=TRUE,xlim1=138.2,xlim2=141.4,xinc=0.1,
## ylim1=1.5,ylim2=2.8,yinc=0.1)


}








