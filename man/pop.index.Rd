\name{pop.index}
\alias{pop.index}
\title{
Calculation of population index
}
\description{Calculates population index of a meteor shower
for a given magnitude data, specified period of days and magnitude values.
}
\usage{
pop.index(data,year, month, day.beg, day.end=day.beg, shw, mag=-6:7)
}
\arguments{
  \item{data}{
 data frame consisting of visual meteor magnitude data.
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
   \item{mag}{
  numeric vector specifying range of magnitudes.
}
}
\details{Cummulative summarized magnitude distribution \emph{Phi(m)} is formed by summing cummulative 
frequencies of all observers for each magnitude class \emph{m}.

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
   \item{day}{factor  Day or interval of days}
   \item{month}{numeric  Month of the year}
   \item{year}{numeric  Year}
   \item{mag}{factor  Range of magnitude values} 
   \item{nINT}{Number of observing time intervals}
   \item{nSHW}{Number of observed meteors belonging to the shower}
   \item{pop.index}{Population index}
   \item{sigma.r}{Standard error of population index}
}
}
\references{
Koschack R. and Rendtel J. (1990b). Determination of
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
the faintest magnitude classes are not included (m<=4 or in exceptional cases m<=5) and there are at least
5 magnitude classes available.  All these conditions are fulfilled for the range of magnitude values printed 
in results.

Argument \code{data} has to consist of the columns named "m6" and "p7". 
}
\seealso{
\code{\link{mag.distr}},\code{\link{zhr}}
}
\examples{
##select visual meteor data for observation of Perseids, time period 1-20th August 2007 
##and calculate population index using magnitudes m<=4
data(magn07)
magnPer<-filter(magn07,shw="PER", year=2007, month=8, day.beg=1, day.end=20)
pop.index(magnPer,year=2007, month=8, day.beg=1, day.end=20, shw="PER",mag=-6:4)
}








