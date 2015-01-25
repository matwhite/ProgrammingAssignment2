## This is a system of functions to allow us to cache the inverse of a matrix.
## Creating and using the cache helps save computational time and resources
## since this type of function is resource-intensive.

## Create an object that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Provide a place to store the cache
    m <- NULL
    ## Fill the cache
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Get the cache
    get <- function() x
    ## Set the matrix
    setmatrix <- function(Solve) m <<- Solve
    ## Get the matrix
    getmatrix <- function() m
    ## Return the methods 
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## Wrap the call to solve the inverse of a matrix in a function
## which first attempts to return a cached copy of prior work done
## before attempting to do the work.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## First see if we already have it in the cache
        m <- x$getmatrix()
        ## If it is in the cache, return the cached data
        if (!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        ## If it is not in the cache
        ## then do the work, cache it for later, and return it
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
