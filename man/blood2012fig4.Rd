\name{blood2012fig4}
\alias{blood2012fig4}
\title{Radiation-to-CML (chronic myeloid leukemia) waiting times}
\description{ This function reproduces Figure 4  of our Blood paper of 2012.}
\usage{blood2012fig4(aBombHome="/data/abomb")} %,N=5,M=3,bkgrnd="exponential")} 

\arguments{
  \item{aBombHome}{ The directory that contains the Japanese A-bomb survivor data. It 
  is assumed that \file{HEMA87.DAT} exists in this directory. }
%  \item{N}{ The number of waiting time distribution bins.}
%  \item{M}{ The number of age at diagnosis (aka attained age) bins. Only used if bkgrnd = "bins" }
%  \item{bkgrnd}{ The number of age at diagnosis (aka attained age) bins. }
}
\details{Times between radiation exposure and onset of CML
appear to be longer in females than in males.}

\note{On linux systems the file \file{HEMA87.DAT} must remain named in capital letters.
  This work was supported by the National Cancer Institute and Tufts Integrative 
        Cancer Biology Program under U54CA149233-029689. }

\references{
Radivoyevitch, T., Hlatky, L., Landaw, J. and Sachs, R. K., 
Quantitative modeling of chronic myeloid leukemia: insights from radiobiology. Blood 119, 4363-4371 (2012).}
\author{Tom Radivoyevitch (\email{txr24@case.edu})}
\seealso{\code{\link{SEERaBomb-package}} }
\examples{
\dontrun{
library(SEERaBomb)
blood2012fig4()
}
}
\keyword{Documentation}
