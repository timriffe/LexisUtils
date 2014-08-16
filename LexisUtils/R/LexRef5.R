
#'
#' @title LexRef5 Overlays semi-transparent 5-year lexis triangle outlines onto a Lexis plot
#' 
#' @description This is an optional overlay function for Lexis map plots.
#' 
#' @param ages vector of ages, e.g. \code{0:110}. These are y values.
#' @param years vector of years, e.g. \code{1900:2010}. These are x values.
#' @param col line color, default \code{"#DDDDDD50"} (transparent gray).
#' @param ... optional arguments passed to \code{segments()}
#' 
#' @export
#' 

LexRef5 <- function(ages, years, col = "#DDDDDD50", ...){
    # vertical
    segments(years[years %% 5 == 0],
            min(ages),
            years[years %% 5 == 0],
            max(ages) + 1,
            col = col,
            ...
    )
    # horiz lexis lines
    segments(min(years),
            ages[ages %% 5 == 0],
            max(years) + 1,
            ages[ages %% 5 == 0],
            col = col,
            ...
    )
    # diag cohort references, bit more tricky:
    l.years <- (min(years) - ages) 
    coh.ext <- range(c(l.years[l.years %% 5 == 0], years[years %% 5 == 0]))
    cohs  <- seq(coh.ext[1], coh.ext[2], by = 5)
    
    # bottom, left:
    xl  <- cohs + min(ages)
    yb  <- rep(min(ages), length(cohs))
    yb[xl < min(years)] <- yb[xl < min(years)] + min(years) - xl[xl < min(years)]
    xl[xl < min(years)] <- min(years)
    
    # top, right:
    xr  <- cohs + max(ages) + 1
    yt  <- rep(max(ages) + 1, length(cohs))
    yt[xr > (max(years) + 1)] <- yt[xr > (max(years) + 1)] - xr[xr > (max(years) + 1)] + (max(years) + 1)
    xr[xr > (max(years) + 1)] <- max(years) + 1
    
    # cut down one last time:
    xr <- xr[yt >= min(ages)]
    xl <- xl[yt >= min(ages)]
    yb <- yb[yt >= min(ages)]
    yt <- yt[yt >= min(ages)]
    
    # draw cohort refs:
    segments(xl, yb, xr, yt, col = col, ...)
}


