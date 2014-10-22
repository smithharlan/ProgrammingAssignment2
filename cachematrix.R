## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. This pair of functions caches the inverse 
## of a matrix.

## The makeCacheMatrix function creates a special matrix which is 
## really a list containing functions to 1) set the value of the 
## matrix, 2) get the value of the matrix, 3) set the value of the
## inverse of the matrix, and 4) get the value of the inverse of 
## the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see 
## if the inverse matrxi has already been calculated. If so, it gets the inverse 
## matrix from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of the inverse matrix in 
## the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
