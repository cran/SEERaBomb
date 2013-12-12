\name{SEERaBomb-package}
\alias{SEERaBomb-package}
\docType{package}
\title{ SEER data setup and examples including analyses of A-Bomb Data }
\description{
This package strives to make SEER data more accessible from R. 
It provides functions that can be combined to convert fixed-width format
SEER ASCII text data files into R binaries. It also provides examples of the use of these binaries and examples that use the Japanese A-bomb survivor data. 
This package works only with the latest SEER release, now of April 24, 2013.
SEER data field positions and names can change over the years, and a goal of 
SEERaBomb is to buffer/protect R scripts from such changes.
}

\details{
\tabular{ll}{
Package: \tab \pkg{SEERaBomb}\cr
Type: \tab Package\cr
Depends: \tab LaF, RSQLite\cr
Suggests: \tab bbmle \cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
LazyData: \tab yes\cr
URL: \tab \url{http://epbi-radivot.cwru.edu/SEERaBomb/SEERaBomb.html}\cr
}

\if{html}{\figure{SEERaBombFig.png}}
\if{latex}{\figure{SEERaBombFig.png}{options: width=5in}}
}


\note{ This work was supported by the National Cancer Institute and Tufts Integrative 
        Cancer Biology Program under U54CA149233-029689. }

\author{ Tom Radivoyevitch (\email{txr24@case.edu}) }
\seealso{\code{\link{getFields},\link{pickFields},\link{mkSEER}} }

\keyword{package}
