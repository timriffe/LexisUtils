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
# all defaults. Colors are meaningless, but perhaps aid to define regions.
# logged by default.
LexisMap(APmatrix)

# increase breaks to make smoother, change z limits from defaults
# (defaults adequate for most human mortality)
LexisMap(APmatrix, nbreaks = 51, zlim = c(1e-4, 1))

# what if you want another color palette?
library(grDevices)    # if LexisUtils is installed, you'll have this already
library(RColorBrewer) # if LexisUtils is installed, you'll have this already
# define a ramp based on well-spaced colors. The default ramp (Spectral colors)
# does not convert to grayscale. They just increase
# surface definition. For widespread sharing, or publication, take one of the palettes
# shown in the FIRST block here:
#display.brewer.all()

# Note the name of the Palette you want, "Greys", for instance,
# and extract a specified nr of colors from it using brewer.pal(),
 then feed these to the color ramp interpolator function:
Greys <- colorRampPalette(brewer.pal(9, "Greys"), space = "Lab")
# Greys() is now a color ramp function. You give it an integer value,
# and it spits back that many evenly-spaced colors over the ramp you provided
# just give it to LexisMap() as the argument 'colramp'
LexisMap(APmatrix, colramp = Greys, contour = TRUE, LexRef = FALSE)
# contours may also increase legibilty, but note that infant mortality
# can be smudged out with the contours themselves. Matter of taste.
# We can also turn off the lexis lines

# the surface need not be logged. Try looking at fertility both logged and unlogged.
LexisMap(APmatrix, log = FALSE)

# plot an age-cohort version of the same (roughly)
# squares are still squares- they don't exactly represent
# age-cohort parallelograms, but sort of. Vertical striations
# will indicate cohort curiosities in this plot. They would
# possibly come out stronger if viewed as AC-grouped data to
# begin with. (or triagles outright)
LexisMap(AP2AC(APmatrix))
}
\author{
  Tim Riffe \email{tim.riffe@gmail.com}
}
\references{
  \url{www.mortality.org}
}
\keyword{data}

