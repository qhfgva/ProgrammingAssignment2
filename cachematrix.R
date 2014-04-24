s file contains 2 functions:
## - makeCacheMatrix creates a closure for utility functions
##   operating on a matrix
## - cacheSolve handles lookups/setting of cached solve() calls


## makeCacheMatrix produces a set of functions for accessing the results
## of a solve() call on a given matrix.  The main benefit is keeping a
## cache of the possibly expensive operation

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## cacheSolve returns the result of calling solve() on a matrix, using
## a cached value if already available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
