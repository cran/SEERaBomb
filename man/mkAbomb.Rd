\name{mkAbomb}
\alias{mkAbomb}
\title{ Make Abomb data binaries }
\description{ This function converts the Abomb data file lsshempy.csv into R binaries.}
\usage{mkAbomb(AbombHome="/data/Abomb")}

\arguments{
  \item{AbombHome}{ The directory that contains the Abomb file \file{lsshempy.csv}.  This should be writable by the user.}
}

\details{This function makes both a binary data.frame file and an SQLite database.}
\value{A dataframe that describes the column names. This function is however mainly called for its side-effects: it produces R binary data files.}
\note{ This function works only with the Abomb data file lsshempy.csv released by RERF in 2013. }
\author{ Tom Radivoyevitch (\email{txr24@case.edu}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkSEER},\link{getFields},\link{pickFields}} }
\examples{
\dontrun{
library(SEERaBomb)
mkAbomb()  
}
}
\keyword{IO}
