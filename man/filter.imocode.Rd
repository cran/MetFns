\name{filter.imocode}
\alias{filter.imocode}
\title{
Selection of visual meteor data by IMO observer code
}
\description{
Selects data for a given visual meteor dataset and specified IMO observer code.
}
\usage{
filter.imocode(data, imocode)
}
\arguments{
  \item{data}{
data frame consisting of visual meteor data (rate or magnitude data).
}
  \item{imocode}{
character string consisting of five capital letters which represent IMO observer code. 
}
}
\details{
IMO observer code is a combination of the first three letters of the last name and the first two letters of the first name.  List of IMO observer codes
can be found in the dataframe \code{\link{vmdbpers}}. IMO observer codes are unique, meaning that if 5-letter combination
is already used, the observer gets modified IMO code.
}
\value{
\code{filter.imocode} returns data frame with the same number of columns as the argument \code{data}, 
containing values corresponding to the observer with specified  IMO code. 
}
\references{
\url{http://www.imo.net/data/visual}

Rendtel J. and Arlt R., editors (2008). \emph{IMO Handbook
For Meteor Observers}. IMO, Potsdam.
}
\author{
Kristina Veljkovic
}
\note{
Argument \code{data} has to consist of the column named "IMOcode".
}
\seealso{
\code{\link{filter}},\code{\link{filter.obsname}}
}
\examples{
## select visual meteor data for 2010. corresponding to the observer with IMO code SAVBR

## rate data for the year 2010
data(rate10)
filter.imocode(rate10,imocode="SAVBR")

## magnitude data for the year 2010
data(magn10)
filter.imocode(magn10,imocode="SAVBR")
}

