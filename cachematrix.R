## Put comments here that give an overall description of what your
## functions do

## Create list of functions which can cache matrix,
## retrieve matrix, cache inverse of matrix,
## and retrieve inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setmean = setinv,
         getmean = getinv)
}


## Retrieves cached value of matrix inverse, if stored
## Otherwise calculates the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("retreiving cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setmean(inv)
    inv
}

