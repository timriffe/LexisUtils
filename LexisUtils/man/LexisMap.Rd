\name{LexisMap}
\alias{LexisMap}
\title{LexisMapAP A simple Age-Period shaded Lexis Contour map}
\usage{
  LexisMap(APmatrix, log = TRUE,
    ages = as.integer(rownames(APmatrix)),
    years = as.integer(colnames(APmatrix)), breaks, col,
    colramp = colorRampPalette(rev(brewer.pal(9, "Spectral")), space = "Lab"),
    nbreaks = 21, LexRef = TRUE, refcol = "#BBBBBB50",
    xlab = "Year", ylab = "Age", zlim, contour = FALSE,
    ...)
}
\arguments{
  \item{APmatrix}{an age-period matrix of rates (usually)
  or some other quantity. Ages in rows, years in columns}

  \item{log}{logical, default = TRUE, should a logged
  surface be plotted - good for mortality data, and
  sometimes for fertility data, depends.}

  \item{ages}{y values, these are taken from the matrix
  \code{rownames()} by default. If the matrix has no
  \code{dimnames()}, then specify this as a vector.}

  \item{years}{x values, these are taken from the matrix
  \code{colnames()} by default. If the matrix has no
  \code{dimnames()}, then specify this as a vector.}

  \item{breaks}{optional vector of value breaks. If
  specified manually, \code{col} must also be given.}

  \item{col}{optional vector of colors. If specified
  manually, \code{breaks} must also be given. \code{col}
  must be 1 shorter than \code{breaks}.}

  \item{colramp}{You can specify an arbitrary color ramp
  function. See examples for tips on changing that. This
  will be ignored if \code{col} is specified.}

  \item{nbreaks}{how many value breaks should be
  calculated? These will be spaced evenly either over the
  values or over the log of the values. Not used if
  \code{breaks} is specified.}

  \item{LexRef}{logical. default \code{TRUE}. Should Lexis
  reference lines be plotted over the surface? These can't
  be added later.}

  \item{refcol}{color of Lexis reference lines.}

  \item{{xlab,}{ylab} same as for \code{plot()}.}

  \item{zlim}{optional user specified value bounds.
  Detected automatically otherwise. Will be different for
  logged vs unlogged data.}

  \item{contour}{logical. Default \code{FALSE}. Should
  contour lines be added at the breaks? These will be black
  and are inflexible.}

  \item{...}{other optional arguments to be passed to
  \code{lattice::levelplot()}.}
}
\value{
  invisible trellis object. It can be \code{print()ed}
  later.
}
\description{
  This is essentially a wrapper to \code{image.plot()},
  with some nice default values for logged mortality data.
  Written rather hastily for Prof. Horiuchi, Sept, 2013. By
  default a simple plot will come out, but you can learn to
  customize it by entering \code{?fields::image.plot}. Ages
  and years are assumed to be in units of 1!
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
  Tim Riffe
}

