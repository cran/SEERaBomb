\name{blood2012fig5a}
\alias{blood2012fig5a}
\title{Chronic myeloid leukemia incidence versus age}
\description{ This function reproduces Figure 5a  of our Blood paper of 2012.
              It uses SEER data collected since 1973.}
\usage{blood2012fig5a(seerHome="/data/SEER")} %,N=5,M=3,bkgrnd="exponential")} 

\arguments{
  \item{seerHome}{ The home directory of the SEER data. }
}
\details{CML is defined here using ICD-O2 histological code 9863. Subsequent to our Blood publication we deduced
 that this code includes CMML (chronic myelomonocytic leukemia). The impact of this is that 
 the true CML aging rate is lower than produced by this function; correct rates based on the 2000-2009 data set
 are produced by \file{CMLincid.R} in \file{papers/activeMS}.  
 }


\note{This work was supported by the National Cancer Institute and Tufts Integrative 
        Cancer Biology Program under U54CA149233-029689. }

\references{
Radivoyevitch, T., Hlatky, L., Landaw, J. and Sachs, R. K., 
Quantitative modeling of chronic myeloid leukemia: insights from radiobiology. Blood 119, 4363-4371 (2012).}
\author{Tom Radivoyevitch (\email{txr24@case.edu})}
\seealso{\code{\link{SEERaBomb-package}} }
\examples{
\dontrun{
library(SEERaBomb)
blood2012fig5a()
}
}
\keyword{Documentation}
