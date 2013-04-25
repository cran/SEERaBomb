\name{blood2012fig6}
\alias{blood2012fig6}
\title{Chronic myeloid leukemia incidence versus age at exposure}
\description{ This function reproduces Figure 6 in our Blood paper of 2012.}
\usage{blood2012fig6(aBombHome="/data/abomb")} %,N=5,M=3,bkgrnd="exponential")} 

\arguments{
  \item{aBombHome}{ The directory that contains the Japanese A-bomb survivor data. It 
  is assumed that \file{HEMA87.DAT} exists in this directory.}
}
\details{With genders and cities pooled, CML has approximately no age-at-exposure dependence.}

\value{None. }

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
blood2012fig6()
}
}
\keyword{Documentation}
