\name{zhr.plot}
\alias{zhr.plot}
\title{
Plot of zenithal hourly rate (ZHR)
}
\description{Plots average zenithal hourly rate of a meteor shower
for data containing ZHR values and given limits on x and y axes..
}
\usage{
zhr.plot(data,xlim1,xlim2,xinc,ylim1,ylim2,yinc)
}
\arguments{
  \item{data}{
  data frame consisting of visual meteor rate data.
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
\value{xy plot of Zenithal Hourly Rate is made. Solar longitude is on x-axis and ZHR on y-axis. ZHR is represented with black filled circles and \emph{68\%} confidence intervals. Values of limits (minimum
and maximum values) on x and y axis (\code{xlim1,xlim2,ylim1,ylim2}), as well as increments between the axis labels
(\code{xinc,yinc}) should be provided to function call.
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
\code{\link{zhr}},\code{\link{pop.index2}}
}
\examples{
## calculate and make plot of ZHR for observations of 2015 Perseids, time period
## 10th to 25th October, min bin size 0.1 degree, max bin size 1 degree, 
## number of meteors equals 100 
## First select rate data for Orionids activity from 10th to 25th October, 
## limiting magnitudes above 5, radiant elevation above 20 degrees, percentage of clouds below 20
ori2015<-filter(rate2015,date.start="2015-10-10",date.end="2015-10-25",shw="ORI",
P.low=0,P.up=20,mag.low=5,mag.up=8,h.low=20,h.up=90)
orizhr<-zhr(ori2015,date.start="2015-10-10",date.end="2015-10-25",shw="ORI",r=2.5,
kmin=0.1,kmax=1,num=100)

## make graphic of ZHR
## x-axis limits: min(sollong)=197.184, max(sollong)=210.224
## y-axis limits: min(ZHR-st.err)=2,max(ZHR+st.err)=29.6
zhr.plot(orizhr,xlim1=197,xlim2=211,xinc=1,ylim1=0,ylim2=30,yinc=2)

}