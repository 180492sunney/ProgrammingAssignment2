## This file contains two functions which in conjunction allows us to compute the inverse of a matrix efficiently.
## If the inverse of a matrix is already computed, it is fetched from the memory and displayed.
## If it is a new matrix, the new inverse is calculated

## This function is used to define new matrix with 4 sub functions to set and get the matrix and also set and get the inverse of that matrix

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
    setinv = setinv,
    getinv = getinv)
    
}


## This function takes an object(matrix) of type makeCacheMatrix as input and returns the inverse of that matrix by fetching it from memory if it is already calculated and if it isn't already calculated the new inverse is calculated on the spot

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
