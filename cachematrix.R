## These functions create a cacheable matrix that can be used to calculate and
## store the inverse such that it only gets calculated once
## 
## Usage:
##   Create a cache and pass it a matrix
##     cache <- makeCacheMatrix(m)
##   Retrieve the inverse of the cached matrix as often as needed
##     cacheSolve(cache)
##   Assign a new matrix to the cache at any time and it will be inversed on
##   the next call to cacheSolve(cache)
##     cache$set(m2)

## Create a cacheable matrix with functions to set and retrieve the matrix
## and its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(solved) inv <<- solved
        
        getInverse <- function() inv
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## Solve the inverse of a cacheable matrix. Return the cached inverse if present
## or else calculate and store the inverse before returning it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mx <- x$get()
        inv <- solve(mx, ...)
        x$setInverse(inv)
        inv
}
