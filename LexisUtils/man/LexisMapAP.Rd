\name{LexisMapAP}
\alias{LexisMapAP}
\title{LexisMapAP A simple Age-Period shaded Lexis Contour map}
\usage{
  LexisMapAP(APmatrix, log = TRUE,
    ages = as.integer(rownames(APmatrix)),
    years = as.integer(colnames(APmatrix)), ...)
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

  \item{...}{other optional arguments to be passed to
  \code{image.plot()}.}
}
\description{
  This is essentially a wrapper to \code{image.plot()},
  with some nice default values for logged mortality data.
  Written rather hastily for Prof. Horiuchi, Sept, 2013. By
  default a simple plot will come out, but you can learn to
  customize it by entering \code{?fields::image.plot}. Ages
  and years are assumed to be in units of 1!
}
\author{
  Tim Riffe
}

