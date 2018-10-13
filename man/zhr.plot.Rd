\name{zhr.plot}
\alias{zhr.plot}
\title{
Plot of zenithal hourly rate (ZHR)
}
\description{Plots average zenithal hourly rate of a meteor shower
for data containing ZHR values and given limits on x and y axes.
}
\usage{
zhr.plot(zhrdata,xlim1,xlim2,xinc,ylim1,ylim2,yinc,
dlim1=NULL,dlim2=NULL,dinc=NULL,dunit=NULL)
}
\arguments{
  \item{zhrdata}{
  data frame consisting of ZHR values.
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
  c vector specifying increment between labels on y-axis.
}
\item{dlim1}{
  character vector specifying start date on secondary x-axis, given in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone).
}
\item{dlim2}{
  character vector specifying end date on secondary x-axis, given in \code{"\%Y-\%m-\%d \%H:\%M:\%S"} format (UTC timezone).
}
\item{dinc}{
  numeric vector specifying increment between labels on secondary x-axis.
}
\item{dunit}{
 character vector specifying unit of increment \code{dinc}. Accepted values are "min" for minute, "h" for hour and "day" for day.
}

}
\details{
\code{zhrdata} represents table of ZHR values calculated using \code{zhr} function.
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
## around maximum, 21th to 25th October, min bin size 0.16 degrees (4 hours), 
## max bin size 1 degree, number of meteors equals 100 
## First select rate data for Orionids activity from 21th to 25th October, 
## limiting magnitudes above 5, radiant elevation above 20 degrees, 
## percentage of clouds below 20
ori2015<-filter(rate2015,date.start="2015-10-21",date.end="2015-10-25",shw="ORI",
P.up=20,mag.low=5,h.low=20)
orizhr<-zhr(ori2015,date.start="2015-10-21",date.end="2015-10-25",shw="ORI",
r=2.5,kmin=0.16,kmax=1,num=100)

## make graphic of ZHR
## x-axis limits: min(sollong)=207.137, max(sollong)=210.276
## y-axis limits: min(ZHR-st.err)=14.2,max(ZHR+st.err)=21.9
zhr.plot(orizhr,xlim1=207,xlim2=211,xinc=1,ylim1=14,ylim2=22,yinc=1)

## change date tick marks on secondary x-axis, to show date at midnight
 zhr.plot(orizhr,xlim1=207,xlim2=211,xinc=1,ylim1=14,ylim2=22,yinc=1,
 dlim1="2015-10-21",dlim2="2015-10-25",dinc=1,dunit="day")

}