################################################################################  
###  This function computes the inverse of a symmetric matrix X with full rank.
###  It proofs if the inverse already exists and in this case returns the 
###  inverse from cache. It consists of two separate functions "makeCacheMatrix"
###  and "cacheSolve".
### 
###  makeCacheMatrix: This function creates a special "matrix" object that can 
###  cache its inverse.
###  
###  cacheSolve: This function computes the inverse of the special "matrix" 
###  returned by makeCacheMatrix above. If the inverse has already been 
###  calculated (and the matrix has not changed), then the cachesolve should 
###  retrieve the inverse from the cache.
### 
###  Important Note: The matrix X must be invertible
###  
###  Usage: 
###  1.  Define a symmetric matrix X 
###  Example: X <- matrix(rnorm(100), nrow = 10, ncol = 10)
###  
###  2. Use cacheSolve to declare  
###  a <- makeCacheMatrix(X)
###  
###  3. Calculate the inverse of X
###  cacheSolve(a)
###
###############################################################################
  



### This function creates a special "matrix" object that can 
### cache its inverse.

makeCacheMatrix <- function(x, ...) {
                            ## Return a matrix that is the inverse of 'x'

                            m <- NULL
                            ## Sets 
                            set <- function(y) {
                                                        x <<- y
                                                        m <<- NULL
                            }
                            get <- function() x
                            
                            setinverse <- function(solve) m <<-solve
                            
                            getinverse <- function() m
                            
                            list(set = set, get = get,
                                 setinverse = setinverse,
                                 getinverse = getinverse)
}


###  cacheSolve: This function computes the inverse of the special "matrix" 
###  returned by makeCacheMatrix above. If the inverse has already been 
###  calculated (and the matrix has not changed), then the cachesolve should 
###  retrieve the inverse from the cache.


cacheSolve <- function(x = matrix()) {
                            
                            m <- x$getinverse()  
                            if(!is.null(m)) { # Test if the inverse of X exists
                                                        message("getting cached data") # 
                                                        return(m)
                            }
                            
                            data <- x$get()  # If inverse of X does not exist
                            
                            m <- solve(data)
                            
                            x$setinverse(m)
                            m
                            
}





