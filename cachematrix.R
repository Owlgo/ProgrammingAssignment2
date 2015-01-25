## Put comments here that give an overall description of what your
## functions do

##  Creates an object that stores a matrix and cache's its inverse. 

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(mtrxinv) inv <<- mtrxinv
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Calculates the inverse of a matrix if the inerse does not exit. If it does exist it uses the existing matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setmean(inv)
        inv
}
