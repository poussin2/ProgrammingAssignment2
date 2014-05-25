## Put comments here that give an overall description of what your
## functions do

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


## This function takes a special matrix and computes its inverse 
## by first looking if the cached value of the inverse is set in which case it returns its value
## Otherwise this function computes the inverse of the special matrix and caches its value 
## for further computation

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("Getting cached inverse")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
