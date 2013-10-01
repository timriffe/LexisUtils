LexisUtils
==========

A small set of functions for working with demographic data matrices and for plotting them
```
# ------------/ some example code to get started \----------------
#library(devtools)
#install_github("LexisUtils", subdir = "LexisUtils", username = "timriffe")
library(LexisUtils)

# loads matrix called 'APmatrix', US male mortality rates in Lexis squares
data(APmatrix) 

# a logged mortality surface
LexisMapAP(APmatrix)

## in case you want to add contours
## Achtung! if you resize the plot, the contours DONT move

ages  <- as.integer(rownames(APmatrix))
years <- as.integer(colnames(APmatrix))

contour(x = years + .5, 
        y = ages+.5,
        z = log(t(APmatrix)),
        add = TRUE, 
        drawlabels = FALSE)
# semi-transparent Lexis reference lines:
LexRef5(years = years, ages = ages)

# or try an age-cohort surface:
LexisMapAP(AP2AC(APmatrix))
```
