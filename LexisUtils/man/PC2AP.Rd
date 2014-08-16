\name{PC2AP}
\alias{PC2AP}
\title{\code{PC2AP} convert a period-cohort matrix to an age-period matrix.}
\usage{
  PC2AP(PCmatrix, agemin = 0, agemax = 110, Lexis = 1)
}
\arguments{
  \item{PCmatrix}{a matrix (period by cohort) of quantities
  (Deaths, Births, Exposures) in triangles, either upper or
  lower. The matrix must either represent lower triangles
  or upper triangles, not both, unless you know what you're
  doing. Whether triangles are upper or lower is indicated
  with the other argument, \code{Lexis}. This function can
  also be used to shift population counts from period to
  age matrices, as long as the population is January 1st or
  December 31st. Matrices must in any case have row names
  and column names, where rows are years (integers, no
  '+'!) and columns are cohorts (also simple integers).}

  \item{Lexis}{either 1 or 2. 1 indicates lower triangles
  and 2 indicates upper triangles. For population counts, 1
  indicates January 1st and 2 indicates December 31st.
  Default is 2, so watch out- if you choose wrong, then
  column labels in the AP matrix output will be off by 1.
  If you're not sure, draw a diagram.}

  \item{agemin}{lower cutoff for age after conversion}

  \item{agemax}{upper bound for age after conversion. A
  long series of PC data will imply some very high ages for
  the first cohorts, so you have to cut off at some
  point... 110 is chosen to be consistent with HMD output,
  but this can be changed.}
}
\value{
  a \code{APmatrix} a matrix in age-period format, with
  correctly labeled dimensions. Note that if the original
  contents were the PC shape that age is the lower left
  corner, and that the shape spans two ages. Draw a picture
  if necessary.
}
\description{
  \code{PC2AP()} is an auxiliary function. Default lexis
  value is 1, so watch out- if you choose wrong, then
  column labels in the AP matrix output will be off by 1.
  If you're not sure, draw a diagram. If using this to
  reshape freshly crafted extinct-cohort-method population
  counts (VV-death-derived), then use Lexis = 1 to get the
  age right. This is not intuitive because TU marks the age
  of the VV shape in the HMD, so you'd think that Lexis = 2
  should be used, but more generally we want this behavior
  for the Lexis argument.
}
\author{
  Tim Riffe \email{triffe@demog.berkeley.edu}
}

