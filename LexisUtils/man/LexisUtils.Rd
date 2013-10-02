\docType{package}
\name{LexisUtils}
\alias{LexisUtils}
\alias{LexisUtils-package}
\title{A small set of utility functions for working with demographic data matrices and plotting.}
\description{
  A small set of utility functions for working with
  demographic data matrices and plotting.
}
\examples{
data(APmatrix)
str(APmatrix)
LexisMapAP(APmatrix)
# a logged mortality surface
LexisMapAP(APmatrix)

## in case you want to add contours
## Achtung! if you resize the plot, the contours DONT move

ages  <- as.integer(rownames(APmatrix))
years <- as.integer(colnames(APmatrix))

contour(x = years + .5,
        y = ages+.5,
        z = log(t(APmatrix)),
        add = TRUE,
        drawlabels = FALSE)
# semi-transparent Lexis reference lines:
LexRef5(years = years, ages = ages)

# or try an age-cohort surface:
LexisMapAP(AP2AC(APmatrix))
}
\author{
  Tim Riffe \email{tim.riffe@gmail.com}
}
\references{
  \url{www.mortality.org}
}
\keyword{data}

