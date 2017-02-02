\name{radiant}
\alias{radiant}
\docType{data}
\title{
Coordinates of radiants of meteor showers
}
\description{
Coordinates of radiants of meteor showers during the year.
}
\usage{radiant}
\format{
  A data frame with 365 observations on the following 58 variables.
  \describe{
    \item{\code{Day}}{numeric  Day of the month}
    \item{\code{Month}}{numeric  Month of the year}
    \item{\code{ANT.Alpha}}{numeric  Right ascension of Antihelion Source radiant}
    \item{\code{ANT.Delta}}{numeric  Declination of Antihelion Source radiant}
    \item{\code{QUA.Alpha}}{numeric  Right ascension of Quadrantids radiant}
    \item{\code{QUA.Delta}}{numeric  Declination of Quadrantids radiant}
    \item{\code{DLM.Alpha}}{numeric  Right ascension of December Leonis Minorids radiant}
    \item{\code{DLM.Delta}}{numeric  Declination of December Leonis Minorids radiant}
    \item{\code{ACE.Alpha}}{numeric  Right ascension of Alpha-Centaurids radiant}
    \item{\code{ACE.Delta}}{numeric  Declination of Alpha-Centaurids radiant}
    \item{\code{GNO.Alpha}}{numeric  Right ascension of Gamma-Normids radiant}
    \item{\code{GNO.Delta}}{numeric  Declination of Gamma-Normids radiant}
    \item{\code{LYR.Alpha}}{numeric  Right ascension of Lyrids radiant}
    \item{\code{LYR.Delta}}{numeric  Declination of Lyrids radiant}
    \item{\code{PPU.Alpha}}{numeric  Right ascension of Pi-Puppids radiant}
    \item{\code{PPU.Delta}}{numeric  Declination of Pi-Puppids radiant}
    \item{\code{ETA.Alpha}}{numeric  Right ascension of Eta-Aquarids radiant}
    \item{\code{ETA.Delta}}{numeric  Declination of Eta-Aquarids radiant}
    \item{\code{ELY.Alpha}}{numeric  Right ascension of Eta-Lyrids radiant}
    \item{\code{ELY.Delta}}{numeric  Declination of Eta-Lyrids radiant}
    \item{\code{JBO.Alpha}}{numeric  Right ascension of June Bootids radiant}
    \item{\code{JBO.Delta}}{numeric  Declination of June Bootids radiant}
    \item{\code{CAP.Alpha}}{numeric  Right ascension of Alpha-Capricornids radiant}
    \item{\code{CAP.Delta}}{numeric  Declination of Alpha-Capricornids radiant}
    \item{\code{SDA.Alpha}}{numeric  Right ascension of Southern Delta-Aquarids radiant}
    \item{\code{SDA.Delta}}{numeric  Declination of Southern Delta-Aquarids radiant}
    \item{\code{PER.Alpha}}{numeric  Right ascension of Perseids radiant}
    \item{\code{PER.Delta}}{numeric  Declination of Perseids radiant}
    \item{\code{PAU.Alpha}}{numeric  Right ascension of Piscis Austrinids radiant}
    \item{\code{PAU.Delta}}{numeric  Declination of Piscis Austrinids radiant}
    \item{\code{KCG.Alpha}}{numeric  Right ascension of Kappa-Cygnids radiant}
    \item{\code{KCG.Delta}}{numeric  Declination of Kappa-Cygnids radiant}
    \item{\code{AUR.Alpha}}{numeric  Right ascension of Alpha-Aurigids radiant}
    \item{\code{AUR.Delta}}{numeric  Declination of Alpha-Aurigids radiant}
    \item{\code{SPE.Alpha}}{numeric  Right ascension of  September Epsilon-Perseids radiant}
    \item{\code{SPE.Delta}}{numeric  Declination of  September Epsilon-Perseids radiant}
    \item{\code{STA.Alpha}}{numeric  Right ascension of Southern Taurids radiant}
    \item{\code{STA.Delta}}{numeric  Declination of Southern Taurids radiant}
    \item{\code{ORI.Alpha}}{numeric  Right ascension of Orionids radiant}
    \item{\code{ORI.Delta}}{numeric  Declination of Orionids radiant}
    \item{\code{DAU.Alpha}}{numeric  Right ascension of Delta-Aurigids radiant}
    \item{\code{DAU.Delta}}{numeric  Declination of Delta-Aurigids radiant}
    \item{\code{EGE.Alpha}}{numeric  Right ascension of Epsilon-Geminids radiant}
    \item{\code{EGE.Delta}}{numeric  Declination of Epsilon-Geminids radiant}
    \item{\code{NTA.Alpha}}{numeric  Right ascension of Northern Taurids radiant}
    \item{\code{NTA.Delta}}{numeric  Declination of Northern Taurids radiant}
    \item{\code{LMI.Alpha}}{numeric  Right ascension of Leo Minorids radiant}
    \item{\code{LMI.Delta}}{numeric  Right ascension of Leo Minorids radiant}
    \item{\code{LEO.Alpha}}{numeric  Right ascension of Leonids radiant}
    \item{\code{LEO.Delta}}{numeric  Declination of Leonids radiant}
    \item{\code{AMO.Alpha}}{numeric  Right ascension of Alpha-Monocerotids radiant}
    \item{\code{AMO.Delta}}{numeric  Declination of Alpha-Monocerotids radiant}
    \item{\code{PHO.Alpha}}{numeric  Right ascension of Phoenicids radiant}
    \item{\code{PHO.Delta}}{numeric  Declination of Phoenicids radiant}
    \item{\code{PUP.Alpha}}{numeric  Right ascension of Puppid/Velids radiant}
    \item{\code{PUP.Delta}}{numeric  Declination of Puppid/Velids radiant}
    \item{\code{GEM.Alpha}}{numeric  Right ascension of Geminids radiant}
    \item{\code{GEM.Delta}}{numeric  Declination of Geminids radiant}
  }
}
\details{
Coordinates of radiants of meteor showers are given on 5-days intervals on IMO site. Natural spline interpolation 
was used to calculate radiant coordinates for in-between days.
}
\source{
Meteor Shower Calendar, \url{http://www.imo.net/observations/methods/visual-observation/}
}

