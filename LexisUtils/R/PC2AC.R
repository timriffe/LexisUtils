#' @title \code{PC2AC} convert a period-cohort (VV) matrix to an age-cohort matrix
#'
#' @description
#' \code{PC2AC} is an auxiliary function.
#' 
#' @param PCmatrix a matrix (period by cohort) of quantities (Deaths, Births, Exposures) in triangles, either upper or lower. The matrix must either represent lower triangles or upper triangles, not both, unless you know what you're doing. Whether triangles are upper or lower is indicated with the other argument, \code{Lexis}. This function can also be used to shift population counts from period to age matrices, as long as the population is January 1st or December 31st. Matrices must in any case have row names and column names, where rows are years (integers, no '+'!) and columns are cohorts (also simple integers).
#' @param Lexis either 1 or 2. 1 indicates lower triangles and 2 indicates upper triangles. For population counts, 1 indicates January 1st and 2 indicates December 31st. Default is 2, so watch out- if you choose wrong, then column labels in the AP matrix output will be off by 1. If you're not sure, draw a diagram.
#' 
#' @return a \code{ACmatrix} a matrix in age-cohort format, with dimensions labeled. It will also have an attribute \code{Lexis}, used for automatic Lexis argument detection in the case that the matrix is placed back into \code{AC2PC()} (actually not yet written). Note that if the original contents were the PC shape that age is the lower left corner, and that the shape spans two ages. Draw a picture if necessary.
#' 
#' @author Tim Riffe \email{triffe@@demog.berkeley.edu}
#' 
#' @export
#' 
#' @importFrom reshape2 melt
#' @importFrom reshape2 acast
#' @importFrom compiler cmpfun
#' 
PC2AC <- cmpfun(function(PCmatrix, Lexis){
  if (missing(Lexis)){
    Lexis         <- ifelse(is.null(attr(PCmatrix, "Lexis")), 1, attr(PCmatrix, "Lexis"))
  }
  longform        <- melt(PCmatrix, varnames = c("Year", "Cohort"), value.name = "value")
  head(longform)
  # assume Year is t+x+1. where 1 is the year.offset. Could also be 0. Test if not sure
  longform$Age    <- longform$Year - longform$Cohort + ifelse(Lexis == 2, -1, 0)
  longform        <- longform[!is.na(longform$value) & longform$Age > 0, ]
  # cast back to Age x Year matrix
  ACmatrix        <- acast(longform, Age ~ Cohort, value.var = "value")
  attr(ACmatrix, "Lexis") <- Lexis
  ACmatrix
})
