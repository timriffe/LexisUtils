
#'
#' @title LexisMapAP A simple Age-Period shaded Lexis Contour map
#' 
#' @description This is essentially a wrapper to \code{image.plot()}, with some nice default values for logged mortality data. Written rather hastily for Prof. Horiuchi, Sept, 2013. By default a simple plot will come out, but you can learn to customize it by entering \code{?fields::image.plot}. Ages and years are assumed to be in units of 1!
#' 
#' @param APmatrix an age-period matrix of rates (usually) or some other quantity. Ages in rows, years in columns
#' @param log logical, default = TRUE, should a logged surface be plotted - good for mortality data, and sometimes for fertility data, depends.
#' @param ages y values, these are taken from the matrix \code{rownames()} by default. If the matrix has no \code{dimnames()}, then specify this as a vector. 
#' @param years x values, these are taken from the matrix \code{colnames()} by default. If the matrix has no \code{dimnames()}, then specify this as a vector. 
#' @param ... other optional arguments to be passed to \code{image.plot()}.
#' 
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom fields image.plot
#' 
#' @author Tim Riffe
#' 
#' @export
#' 

LexisMapAP <- function(APmatrix, 
        log = TRUE, 
        ages = as.integer(rownames(APmatrix)),     # by default, this will assume your input matrix
        years = as.integer(colnames(APmatrix)),    # is age x year, with dimensions labeled
        # default color function
        ...
){
    moreArgs     <- list(...)
    # settings optimized for mortality, rather rigid
    if (log){
        # user can override by specifying args via ...
        colRamp         <- colorRampPalette(rev(brewer.pal(9, "Spectral"), space = "Lab"))
        brks            <- seq(-10, 1, by = .2)
        ticks           <- 10 ^ (-5:1) %o% seq(1:9)
        labs            <- ticks
        labs[, c(3,4,6,7,8,9)] <- "" 
        labs            <- c(t(labs))
        ticks           <- c(t(ticks))
        keep            <- log(ticks) >= min(brks) & log(ticks) <= max(brks)
        labs            <- labs[keep]
        ticks           <- log(ticks[keep])
        
        # give Mx some limits
        APmatrix                    <- log(APmatrix)
        APmatrix[APmatrix < -10]    <- -10
        APmatrix[APmatrix > 1]      <- 1
        # this is the default call
        Arguments <- list(x=years + .5, 
                y=ages + .5 ,
                z=t(APmatrix), 
                asp = 1, 
                xlim = c(range(years) + c(0, 1)), 
                ylim = c(range(ages) + c(0, 1)),
                zlim = range(brks),
                col = colRamp(length(brks) - 1),
                useRaster = TRUE, 
                axis.args = list(at = ticks, labels = labs),
                main = "",
                xlab = "Year",
                ylab = "Age")
        # but we can weed- out the stuff the user wanted specific
        if (length(moreArgs)>0){
            Arguments <- c(Arguments[! names(Arguments) %in% names(moreArgs)], moreArgs)
        }
        
        do.call(image.plot, Arguments)
    } else {
        # fewer defaults for not-logged case
        image.plot(years + .5, 
                ages + .5 ,
                t(APmatrix), 
                asp = 1, 
                useRaster = TRUE,
                xlim = c(range(years) + c(0, 1)), 
                ylim = c(range(ages) + c(0, 1)), ...)   
    }
}

#' APmatrix AP matrix of US male mortality rates, ages 0-110+, 1933-2010 (HMD)
#'
#' @name APmatrix
#' @docType data
#' @references \url{www.mortality.org}
#' @keywords data
#'
NULL

#' A small set of utility functions for working with demographic data matrices and plotting.
#'
#' @name LexisUtils
#' @docType package
#' @author Tim Riffe \email{tim.riffe@@gmail.com}
#' @references \url{www.mortality.org}
#' @keywords data
#' @examples 
#' data(USmx)
#' str(USmx)
#' LexisMapAP(USmx)
#' 
#' 
NULL