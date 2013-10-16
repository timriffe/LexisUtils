
#'
#' @title LexisMapAP A simple Age-Period shaded Lexis Contour map
#' 
#' @description This is essentially a wrapper to \code{image.plot()}, with some nice default values for logged mortality data. Written rather hastily for Prof. Horiuchi, Sept, 2013. By default a simple plot will come out, but you can learn to customize it by entering \code{?fields::image.plot}. Ages and years are assumed to be in units of 1!
#' 
#' @param APmatrix an age-period matrix of rates (usually) or some other quantity. Ages in rows, years in columns
#' @param log logical, default = TRUE, should a logged surface be plotted - good for mortality data, and sometimes for fertility data, depends.
#' @param ages y values, these are taken from the matrix \code{rownames()} by default. If the matrix has no \code{dimnames()}, then specify this as a vector. 
#' @param years x values, these are taken from the matrix \code{colnames()} by default. If the matrix has no \code{dimnames()}, then specify this as a vector. 
#' @param breaks optional vector of value breaks. If specified manually, \code{col} must also be given.
#' @param col optional vector of colors. If specified manually, \code{breaks} must also be given. \code{col} must be 1 shorter than \code{breaks}.
#' @param colramp You can specify an arbitrary color ramp function. See examples for tips on changing that. This will be ignored if \code{col} is specified.
#' @param nbreaks how many value breaks should be calculated? These will be spaced evenly either over the values or over the log of the values. Not used if \code{breaks} is specified.
#' @param LexRef logical. default \code{TRUE}. Should Lexis reference lines be plotted over the surface? These can't be added later.
#' @param refcol color of Lexis reference lines.
#' @param {xlab, ylab} same as for \code{plot()}.
#' @param zlim optional user specified value bounds. Detected automatically otherwise. Will be different for logged vs unlogged data.
#' @param contour logical. Default \code{FALSE}. Should contour lines be added at the breaks? These will be black and are inflexible.
#' @param ... other optional arguments to be passed to \code{lattice::levelplot()}.
#' 
#' @return invisible trellis object. It can be \code{print()ed} later.
#' 
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @importFrom lattice levelplot
#' 
#' @author Tim Riffe
#' 
#' @export
#' 
#' @examples 
#' data(APmatrix)
#' str(APmatrix)
#' # all defaults. Colors are meaningless, but perhaps aid to define regions.
#' # logged by default.
#' LexisMap(APmatrix)
#' 
#' # increase breaks to make smoother, change z limits from defaults 
#' # (defaults adequate for most human mortality)
#' LexisMap(APmatrix, nbreaks = 51, zlim = c(1e-4, 1))
#' 
#' # what if you want another color palette?
#' library(grDevices)    # if LexisUtils is installed, you'll have this already
#' library(RColorBrewer) # if LexisUtils is installed, you'll have this already
#' # define a ramp based on well-spaced colors. The default ramp (Spectral colors)
#' # does not convert to grayscale. They just increase
#' # surface definition. For widespread sharing, or publication, take one of the palettes
#' # shown in the FIRST block here:
#' #display.brewer.all()
#' 
#' # Note the name of the Palette you want, "Greys", for instance,
#' # and extract a specified nr of colors from it using brewer.pal(),
#'  then feed these to the color ramp interpolator function:
#' Greys <- colorRampPalette(brewer.pal(9, "Greys"), space = "Lab")
#' # Greys() is now a color ramp function. You give it an integer value, 
#' # and it spits back that many evenly-spaced colors over the ramp you provided
#' # just give it to LexisMap() as the argument 'colramp'
#' LexisMap(APmatrix, colramp = Greys, contour = TRUE, LexRef = FALSE)
#' # contours may also increase legibilty, but note that infant mortality
#' # can be smudged out with the contours themselves. Matter of taste.
#' # We can also turn off the lexis lines
#' 
#' # the surface need not be logged. Try looking at fertility both logged and unlogged.
#' LexisMap(APmatrix, log = FALSE)
#' 
#' # plot an age-cohort version of the same (roughly)
#' # squares are still squares- they don't exactly represent
#' # age-cohort parallelograms, but sort of. Vertical striations
#' # will indicate cohort curiosities in this plot. They would
#' # possibly come out stronger if viewed as AC-grouped data to
#' # begin with. (or triagles outright)
#' LexisMap(AP2AC(APmatrix))
#' 
LexisMap <- function(APmatrix, 
        log = TRUE, 
        ages = as.integer(rownames(APmatrix)), 
        years = as.integer(colnames(APmatrix)),
        breaks,
        col,
        colramp = colorRampPalette(rev(brewer.pal(9, "Spectral")), space = "Lab"),
        nbreaks = 21,
        LexRef = TRUE,
        refcol = "#BBBBBB50",
        xlab = "Year",
        ylab = "Age",
        zlim,
        contour = FALSE,
        ...){
    zs <- 1 / (10^(15:(-15)))
    APmatrix[is.infinite(APmatrix)] <- NA
    if (missing(zlim) & log){
        minz <- min(APmatrix, na.rm = TRUE)
        maxz <- max(APmatrix, na.rm = TRUE)
        zlim <- c(max(zs[zs < minz]), min(zs[zs > maxz]))
    }
    if (missing(zlim) & !log){
        zlim <- range(pretty(APmatrix))
    }
    
    # some checks for manual color and break specification
    if ((!missing(breaks) & missing(col)) |(missing(breaks) & !missing(col))){
        stop("if either breaks or col is specified, then both must be specified")
    }
    
    if (!missing(breaks) & !missing(col)){
        if (length(breaks)-length(col) != 1){
            stop("breaks must be a vector 1 element longer than col")
        }
    }
    
    if (missing(col)){
        col        <- colramp(nbreaks - 1)
    }
    
    if (missing(breaks) & log){
        
        legendlabsd <- c(10,1, .1, .01,  1e-3, 1e-4, 1e-5,1e-6)
        legendlabs  <- c("10","1",".1",".01",".001",".0001",".00001",".000001")[order(legendlabsd)]
        legendlabsd <- sort(legendlabsd)
        keepers     <- legendlabsd >= zlim[1] & legendlabsd <= zlim[2]
        legendlabsd <- legendlabsd[keepers]
        legendlabs  <- legendlabs[keepers]
        
        ticks       <- sort(unique(c(outer(seq(from=1,to=.1,by=-.1),10^(5:(-1)),"/"))))
        ticklabs    <- rep("",length(ticks))
        keepers     <- ticks >= zlim[1] & ticks <= zlim[2]
        ticks       <- ticks[keepers]
        ticklabs    <- ticklabs[keepers]
        
        ticklabs[ticks %in% legendlabsd]    <- legendlabs[legendlabsd%in%ticks]
        
        APmatrix[APmatrix == 0]             <- NA
        APmatrix[APmatrix < zlim[1]]        <- zlim[1]
        APmatrix[APmatrix > zlim[2]]        <- zlim[2]
        APmatrix                            <- log(APmatrix)
        
        breaks      <- approx(log(zlim), n = nbreaks)$y
        ticks       <- log(ticks)
        
    }
    if (missing(breaks) & !log){
        # nice breaks for unlogged data?
        APmatrix[APmatrix < zlim[1]]        <- zlim[1]
        APmatrix[APmatrix > zlim[2]]        <- zlim[2]
        ticklabs    <- ticks       <- pretty(APmatrix, 10) # still only gives a rough number
        breaks      <- approx(zlim, n = nbreaks)$y
    }
    if (!missing(breaks))
        if (LexRef){
            LexRefN <- function(...) { 
                N           <- 5
                subscripts  <- FALSE
                x           <- years
                y           <- ages
                col         <- refcol
                panel.levelplot(...) 
                # vertical
                
                panel.segments(x0=x[x %% N == 0],
                        y0=min(y),
                        x1=x[x %% N == 0],
                        y1=max(y) + 1,
                        col = col,
                        subscripts=subscripts,
                        ...
                )
                # horiz lexis lines
                panel.segments(x0=min(x),
                        y0=y[y %% N == 0],
                        x1=max(x) + 1,
                        y1=y[y %% N == 0],
                        col = col,
                        subscripts=subscripts,
                        ...
                )
                # diag cohort references, bit more tricky:
                l.x <- (min(x) - y) 
                coh.ext <- range(c(l.x[l.x %% N == 0], x[x %% N == 0]))
                cohs  <- seq(coh.ext[1], coh.ext[2], by = N)
                
                # bottom, left:
                xl  <- cohs + min(y)
                yb  <- rep(min(y), length(cohs))
                yb[xl < min(x)] <- yb[xl < min(x)] + min(x) - xl[xl < min(x)]
                xl[xl < min(x)] <- min(x)
                
                # top, right:
                xr  <- cohs + max(y) + 1
                yt  <- rep(max(y) + 1, length(cohs))
                yt[xr > (max(x) + 1)] <- yt[xr > (max(x) + 1)] - xr[xr > (max(x) + 1)] + (max(x) + 1)
                xr[xr > (max(x) + 1)] <- max(x) + 1
                
                # cut down one last time:
                xr <- xr[yt >= min(y)]
                xl <- xl[yt >= min(y)]
                yb <- yb[yt >= min(y)]
                yt <- yt[yt >= min(y)]
                
                # draw cohort refs:
                panel.segments(x0=xl, y0=yb, x1=xr, y1=yt, col = col, subscripts=subscripts,...)
            } 
        } else {
            # this lets the plot carry forward without writing anything on it
            LexRefN <- function(...){
                panel.levelplot(...) 
            }
        }
# panel function for inside levelplot():
    LexisImage <-
            levelplot(x=t(APmatrix),
                    row.values = years + .5,
                    column.values = ages + .5,
                    xlim = range(years) + c(0, 1),
                    ylim = range(ages) + c(0, 1),
                    at = breaks,
                    col.regions = col,
                    colorkey = list(at = breaks,
                            labels = list(at = ticks,
                                    labels = ticklabs)
                    ),
                    scales = list(x = list(at = years[years %% 10 == 0],
                                    labels = years[years %% 10 == 0]),
                            y = list(at = ages[ages %% 10 == 0]),
                            labels = ages[ages %% 10 == 0]),
                    panel = LexRefN,
                    xlab = xlab,
                    ylab = ylab,
                    contour = contour,
                    ...
            )
    print(LexisImage)
    invisible(LexisImage)
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
#' data(APmatrix)
#' str(APmatrix)
#' # all defaults. Colors are meaningless, but perhaps aid to define regions.
#' # logged by default.
#' LexisMap(APmatrix)
#' 
#' # increase breaks to make smoother, change z limits from defaults 
#' # (defaults adequate for most human mortality)
#' LexisMap(APmatrix, nbreaks = 51, zlim = c(1e-4, 1))
#' 
#' # what if you want another color palette?
#' library(grDevices)    # if LexisUtils is installed, you'll have this already
#' library(RColorBrewer) # if LexisUtils is installed, you'll have this already
#' # define a ramp based on well-spaced colors. The default ramp (Spectral colors)
#' # does not convert to grayscale. They just increase
#' # surface definition. For widespread sharing, or publication, take one of the palettes
#' # shown in the FIRST block here:
#' #display.brewer.all()
#' 
#' # Note the name of the Palette you want, "Greys", for instance,
#' # and extract a specified nr of colors from it using brewer.pal(),
#'  then feed these to the color ramp interpolator function:
#' Greys <- colorRampPalette(brewer.pal(9, "Greys"), space = "Lab")
#' # Greys() is now a color ramp function. You give it an integer value, 
#' # and it spits back that many evenly-spaced colors over the ramp you provided
#' # just give it to LexisMap() as the argument 'colramp'
#' LexisMap(APmatrix, colramp = Greys, contour = TRUE, LexRef = FALSE)
#' # contours may also increase legibilty, but note that infant mortality
#' # can be smudged out with the contours themselves. Matter of taste.
#' # We can also turn off the lexis lines
#' 
#' # the surface need not be logged. Try looking at fertility both logged and unlogged.
#' LexisMap(APmatrix, log = FALSE)
#' 
#' # plot an age-cohort version of the same (roughly)
#' # squares are still squares- they don't exactly represent
#' # age-cohort parallelograms, but sort of. Vertical striations
#' # will indicate cohort curiosities in this plot. They would
#' # possibly come out stronger if viewed as AC-grouped data to
#' # begin with. (or triagles outright)
#' LexisMap(AP2AC(APmatrix))
#' 
NULL