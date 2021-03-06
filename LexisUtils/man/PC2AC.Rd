% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/PC2AC.R
\name{PC2AC}
\alias{PC2AC}
\title{\code{PC2AC} convert a period-cohort (VV) matrix to an age-cohort matrix}
\usage{
PC2AC(PCmatrix, Lexis)
}
\arguments{
\item{PCmatrix}{a matrix (period by cohort) of quantities (Deaths, Births, Exposures) in triangles, either upper or lower. The matrix must either represent lower triangles or upper triangles, not both, unless you know what you're doing. Whether triangles are upper or lower is indicated with the other argument, \code{Lexis}. This function can also be used to shift population counts from period to age matrices, as long as the population is January 1st or December 31st. Matrices must in any case have row names and column names, where rows are years (integers, no '+'!) and columns are cohorts (also simple integers).}

\item{Lexis}{either 1 or 2. 1 indicates lower triangles and 2 indicates upper triangles. For population counts, 1 indicates January 1st and 2 indicates December 31st. Default is 2, so watch out- if you choose wrong, then column labels in the AP matrix output will be off by 1. If you're not sure, draw a diagram.}
}
\value{
a \code{ACmatrix} a matrix in age-cohort format, with dimensions labeled. It will also have an attribute \code{Lexis}, used for automatic Lexis argument detection in the case that the matrix is placed back into \code{AC2PC()} (actually not yet written). Note that if the original contents were the PC shape that age is the lower left corner, and that the shape spans two ages. Draw a picture if necessary.
}
\description{
\code{PC2AC} is an auxiliary function.
}
\author{
Tim Riffe \email{triffe@demog.berkeley.edu}
}

