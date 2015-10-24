## These functions were designed to enable matrix creation,
## as well as caching of its inverse.

## Creates a matrix that will have its inverse cached, lazily.

makeCacheMatrix <- function(x = matrix()) {
    inverse = NULL
    
    get = function() {
        x
    }
    
    set = function(newX) {
        x <<- newX
        inverse <<- NULL
    }
    
    getInverse = function() {
        inverse
    }
    
    setInverse = function(newInverse) {
        inverse <<- newInverse
    }
    
    list(get = get,
         set = set,
         getInverse = getInverse,
         setInverse = setInverse)
}


## Caches the inverse of a matrix if it wasn't yet computed,
## otherwise simply returns it.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if (!is.null(inverse))
    {
        return(inverse)
    }
    
    inverse <- solve(x$get(), ...)
    x$setInverse(inverse)
    inverse
}
