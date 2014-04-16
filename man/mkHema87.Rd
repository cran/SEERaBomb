\name{mkHema87}
\alias{mkHema87}
\title{ Make Hema87 Abomb data binaries }
\description{ This function converts the Abomb data file HEMA87.DAT into R binaries.}
\usage{mkHema87(AbombHome="~/data/Abomb")}

\arguments{
  \item{AbombHome}{ The directory that contains the Abomb file \file{HEMA87.DAT}.  This should be writable by the user.}
}

\details{This function makes both a binary data.frame file and an SQLite database.}
\value{ This function is called for its side-effects: it produces R binary data files.}
\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkSEER},\link{getFields},\link{pickFields}} }
\examples{
\dontrun{
library(SEERaBomb)
mkHema87()  
}
}
\keyword{IO}
