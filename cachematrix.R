## The example that comes with the assignment is a good reference as to how
## these functions should be implemented.
##
## Required are...
## 1. A function factory / caching utility, to get/set/serve the cached data
## 2. A function that utilizes the above to fetch or calculate the inverse
##
## Again, the example functions serve a good baseline... simply replace the
## context "mean" with "solve"
##
##
##  ** there is no error checking against non-square nor non-invertible **
##  ** matrices... relying on the assumptions per the assignment        **
##
## Verified via: https://class.coursera.org/rprog-011/forum/thread?thread_id=405



## Following is a function factory that allows the caller work with a
## matrix-wrapper providing facilities to get/set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                  ## ensure result is initialized to *something*

    ## Following are a set of utility functions to...
    set <- function(y) {       ## ... store the given matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x        ## ... return the given matrix

    setinv <- function(inv) {  ## ... set the calculated inverse in cache
        m <<- inv
    }
    getinv <- function() m     ## ...return the cached value


    ## Return the list of functions to be used by callers
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## Following function is expected to take a "special" matrix as an argument
## and return its inverse.  If the inverse is already calculated, it pulls the
## result from the cache; otherwise, it's calculated in here and stored in the
## cache.
##
## "special" matrix is one created using the makeCacheMatrix function above;
## i.e. slightly more involved than the default R matrix class with its utility
## functions

cacheSolve <- function(x, ...) {
    m <- x$getinv()                     ## see if an inverse exists
    if(!is.null(m)) {                   ## it exist -> return the value in cache
        message("getting cached data")
        return(m)
    }

    ## If we are here, the inverse has not been calcuated before
    sourceMatrix <- x$get()             ## get the matrix using utility
    m <- solve(sourceMatrix, ...)       ## calculate its inverse using solve()
    x$setinv(m)                        ## set the value in cache for future use
    m
}
