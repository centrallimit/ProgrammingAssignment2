###############################################################################
# Author: Andreas Kunert
# Date: 2015/12/19
# Email: akunert.statistik@gmail.com
#  
# This function computes the inverse of a well-defined n x n matrix with full
# column  
#
###############################################################################
###############################################################################


makeVector <- function(x = numeric()) {
                            m <- NULL
                            set <- function(y) {
                                                        x <<- y
                                                        m <<- NULL
                            }
                            get <- function() x
                            setmean <- function(mean) m <<- mean
                            getmean <- function() m
                            list(set = set, get = get,
                                 setmean = setmean,
                                 getmean = getmean)
}
