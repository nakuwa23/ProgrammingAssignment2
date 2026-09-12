## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve calculates the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated, it retrieves
## it from the cache instead of recalculating it.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    if (!is.null(inv)) {
        message("Getting cached data...")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}