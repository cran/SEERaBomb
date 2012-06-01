\name{blood2012fig5bc}
\alias{blood2012fig5bc}
\title{Total chromosome translocation prevalence versus age}
\description{ This function reproduces Figures 5b and 5c of our Blood paper of 2012.}
\usage{blood2012fig5bc()} 

%\arguments{none}

\details{
This function fits both 
exponential and power law functions to the total chromosome translocations versus age 
data of Sigurdson et al 2008. 
}

%\value{none }

\note{This work was supported by the National Cancer Institute and Tufts Integrative 
        Cancer Biology Program under U54CA149233-029689. }

\references{
Radivoyevitch, T., Hlatky, L., Landaw, J. and Sachs, R. K., 
Quantitative modeling of chronic myeloid leukemia: insights from radiobiology. Blood 119, 4363-4371 (2012).

Sigurdson et al. Mutation Research 652 (2008) 112-121 (data extracted by \code{plotDigitizer}).
}
\author{Tom Radivoyevitch (\email{txr24@case.edu})}
\seealso{\code{\link{Sigurdson},\link{blood2012fig5a},\link{blood2012fig4}} }
\examples{
\dontrun{
library(SEERaBomb)
blood2012fig5bc()
}
}
\keyword{Documentation}
