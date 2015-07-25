## makeCacheMatrix and cacheSolve functions yield the computation of 
## the inverse of a matrix 'x'

## makeCacheMatrix creates a matrix object that caches the inverse of 'x'

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inverse <- function(solve) inv <<- solve
        get_inverse<- function() inv
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

}


## cacheSolve determines if an inverse has been previously calculated for 'x'
## and if so, will return the value from the cache. If no inverse exists, 
## the function computes the inverse of the matrix

cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$set_inverse(inv)
        inv
}

