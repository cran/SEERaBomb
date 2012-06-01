\name{mkSEER}
\alias{mkSEER}
\title{ Make SEER data binaries }
\description{ This function converts large SEER ASCII text data files into smaller R binaries.}
\usage{mkSEER(df,seerHome="/data/SEER",dataset=c("yr2000_2009","yr1973_2009",
                          "yr1992_2009"),SQL=FALSE)}

\arguments{
  \item{df}{ A data frame that was the output of \code{pickFields}. This determines which fields to transfer. }
  \item{seerHome}{ The directory that contains the SEER \file{population} and \file{incidence} directories. 
               This should be writable by the user.}
  \item{dataset}{The SEER dataset to use. These are uniquely identified by the starting year, 1973 = SEER 9, 1992 = SEER 13,
                 and 2000 = SEER 18, where in SEER X, X is the number of SEER registries. The options are
                 \code{c("yr2000_2009","yr1973_2009",} \code{"yr1992_2009")} and the choice
                 determines the subdirectory of \file{incidence} and \file{population} to use as well as the
                 name of the subdirectory of \code{seerHome} to which the R data files will be written.  }
  \item{SQL}{TRUE if an SQLite database is to be created and populated with tables. The single file \file{all.db} produced
             in this case can be significantly larger than the sum of the \file{*.RData} files also produced. 
             Use of this option is experimental so the default is FALSE.}           
}
\details{This function uses the R package \pkg{LaF} to access the fixed-width format data files 
of SEER. \pkg{LaF} is fast, but it requires knowledge of all the widths of columns wanted, as well as the
the widths of unwanted stretches in between. This knowledge is produced  
by \code{getFields()} and \code{pickFields()} combined. It is passed to \code{mkSEER} as the argument \code{df}.
}
\value{None. This function is called for its side-effects: it produces R binary data files.}

\note{ This version of \pkg{SEERaBomb} works only with the SEER data release of April 2012.
Using the default field choices of \code{pickFields()},
computing times range from ~10 seconds for 1992-2009 processing without SQLite database generation, to 
~90 seconds for 1973-2009 processing with SQLite database generation.
This work was supported by the National Cancer Institute and Tufts Integrative 
Cancer Biology Program under U54CA149233-029689. }
\author{ Tom Radivoyevitch (\email{txr24@case.edu}) }
\seealso{\code{\link{SEERaBomb-package},\link{getFields},\link{pickFields}} }
\examples{
\dontrun{
library(SEERaBomb)
(df=getFields())
(df=pickFields(df))
mkSEER(df,dataset="yr1992_2009")  # fastest option
# the following takes ~4 minutes, but may only need to be done once per year, with each release.
mkSEER(df,dataset="yr1992_2009",SQL=TRUE) 
mkSEER(df,dataset="yr1973_2009",SQL=TRUE)
mkSEER(df,dataset="yr2000_2009",SQL=TRUE)
}
}
\keyword{IO}
