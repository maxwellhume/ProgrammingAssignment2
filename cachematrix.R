## Maxwell Hume
## Submission for Coursera's R Programming (Roger Peng), Assignment 2

## Provides code for storing and computing the inverse of an invertible
## matrix,  using caching where possible.

## Creates an object that stores an invertible matrix,
## as well as its inverse as a special property.
## The matrix must be square and also invertible.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        inv <- inverse
    }
    getinverse <- function() {
        inv
    }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# Performs the solve function to provide the inverse
# of an invertible matrix. Also uses caching to provide
# the answer if it has already been computed.
# The input x must be a cached matrix object
# as produced by the makeCacheMatrix function above.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
