## Simple caching of matrix inverse.
## Usage:
## x<-makeCacheMatrix(a) # a must be an invertible matrix
## cacheSolve(x) # calculates the inverse of a and caches it. 
## cacheSolve(x) # retrieves the inverse from the cache

## Creates a special "matrix" object that can cache its inverse.
## Assumes that x is invertible.
makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
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

## Computes the inverse of the matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
