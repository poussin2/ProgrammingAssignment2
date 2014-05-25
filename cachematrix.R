## This function creates a special matrix that exposes methods to get and 
## set its value and that of its inverse 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function takes a special matrix and computes its inverse.
## If the inverse was previously computed it is simply looked up
## in the cache, otherwise it is computed, cached and returned

cacheSolve <- function(x, ...) {
    ## Look up the inverse of 'x' in the cache
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached inverse")
        return(inverse)
    }
    data <- x$get()
    ## Computing the inverse
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
