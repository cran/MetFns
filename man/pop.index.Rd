\name{pop.index}
\alias{pop.index}
\title{
Calculation of population index
}
\description{Calculates and plots population index of a meteor shower
for a given magnitude data, specified period of days, magnitude values and bin size.
}
\usage{
pop.index(data,year,month.beg,month.end=month.beg,day.beg,day.end=day.beg,
time.beg=0,time.end=2359,shw,mag.range=-6:7,k,add.plot=FALSE,xlim1=NULL,
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
numeric vector(0-2359) specifying upper boundary of time in hours and minutes, corresponding to \code{day.end}. By default, \code{time.end} is set to be equal to 2359.
}

   \item{shw}{
 character string consisting of three capital letters which represent meteor shower code.
}
   \item{mag.range}{
  numeric vector specifying range of magnitudes. It should consist of at least 5 magnitude classes.
}
\item{k}{
  numeric vector specifying bin size in degrees of solar longitude.
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
\details{Probabilities of perception are incorporated in magnitude distributions for each observing interval.
Cummulative summarized magnitude distribution \emph{Phi(m)} is formed by summing cummulative frequencies of all observers for each magnitude class \emph{m}.

Using the relationship for population index \emph{r=Phi(m+1)/Phi(m)} and substitutiong \emph{0,1,...m} magnitudes, 
equation \emph{Phi(m)=Phi(0)r^m} (or \emph{ln(Phi(m))=ln(Phi(0))+r log(m)} in logarithmic form) can be written.
Then, population index \emph{r} is calculated by the method of least squares, for chosen range of magnitude values.

Standard error of population index is approximated with

\emph{sigma_r= r sqrt(sum e_i^2/((n-2)sum_i m_i^2))},

where \emph{i=1,2,..n}, \emph{n} is number of magnitude values, \emph{e_i} regression residuals, 
\emph{i=1,2,..n}.

}
\value{
Data frame containing following vectors
\describe{
   \item{start}{factor  Calendar date and time in UTC at the time interval beginning}
   \item{stop}{factor  Calendar date and time in UTC at the time interval end}
   \item{sollong}{Solar longitude corresponding to the middle of time interval}
   \item{mag}{factor  Range of magnitude values}
   \item{nINT}{Number of observing time intervals}
   \item{nSHW}{Number of observed meteors belonging to the shower}
   \item{pop.index}{Population index}
   \item{sigma.r}{Standard error of population index}
}
If \code{add.plot} is TRUE, additionally xy plot of population index is made. Solar longitude is on x-axis and population index on y-axis. Population index is represented with black filled circles and \emph{65\%} confidence intervals. Values of limits (minimum and maximum values) on x and y axis (\code{xlim1,xlim2,ylim1,ylim2}), as well as increments between the axis labels (\code{xinc,yinc}) should be provided to function call. 
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
}
\seealso{
\code{\link{mag.distr}},\code{\link{pop.index2}},\code{\link{zhr}}
}
\examples{
## calculate and plot population index for observations of 1997 Perseids, time period 
## 28th July to 16th August, radiant elevation higher than 20 degrees,
## total correction smaller than 5, bin size 1 degree
## data(magn97)
## magn<-filter(magn97,year=1997,month.beg=7,month.end=8,day.beg=28,day.end=16,
## shw="PER",h.low=20)
## pop.index(magn,year=1997,month.beg=7,month.end=8,day.beg=28,day.end=16,shw="PER",k=1)

## make graphic of population index
##x-axis limits: min(sollong)=125.5, max(sollong)=143.5
##y-axis limits: min(pop.index-sigma.r)=1.99,max(pop.index+sigma.r)=2.8
## pop.index(magn,year=1997,month.beg=7,month.end=8,day.beg=28,day.end=16,shw="PER",
## k=1,add.plot=TRUE,xlim1=125,xlim2=144,xinc=1,ylim1=1.9,ylim2=2.9,yinc=0.1)
}








