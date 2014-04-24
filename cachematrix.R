## Using R's scoping rule, this function cached the inverse matrix 
## inside R's object up one its initial request.

## This function return a wrapper around a given matrix(m) with accessor
## methods to its attributes, matrix and its inverse.
## =====================================================================
makeCacheMatrix <- function(m = matrix()) {
    # cached inverse of x
    cachedInverse <- NULL
    
    # Set the matrix attribute
    set <- function(newM) {
        m <<- newM
        cachedInverse <<- NULL
    }
    
    # Get the matrix attribute
    get <- function() {
        m
    }
    
    # Set the cached inverse of the matrix
    setInverse <- function(newInverse) {
        cachedInverse <<- newInverse
    }
    
    # Get the cached inverse of the matrix attribute
    getInverse <- function() {
        cachedInverse
    }
    
    # Manifest all accessor methods
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
## ===========================================================================
cacheSolve <- function(m, ...) {
    cachedInverse <- m$getInverse()
    
    toReturn <- if (!is.null(cachedInverse)) {
        message("getting cached inverse matrix")
        cachedInverse 
    }
    else {
        inversed <- solve(m$get())
        m$setInverse(inversed)
        inversed
    }
}