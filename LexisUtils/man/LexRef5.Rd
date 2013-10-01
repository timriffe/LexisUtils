\name{LexRef5}
\alias{LexRef5}
\title{LexRef5 Overlays semi-transparent 5-year lexis triangle outlines onto a Lexis plot}
\usage{
  LexRef5(ages, years, col = "#DDDDDD50", ...)
}
\arguments{
  \item{ages}{vector of ages, e.g. \code{0:110}. These are
  y values.}

  \item{years}{vector of years, e.g. \code{1900:2010}.
  These are x values.}

  \item{col}{line color, default \code{"#DDDDDD50"}
  (transparent gray).}

  \item{...}{optional arguments passed to
  \code{segments()}}
}
\description{
  This is an optional overlay function for Lexis map plots.
}

