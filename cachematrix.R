## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. 


## makeCacheMatrix function performs inverse of a matrix and caches the result

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- solve(inv)
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns a matrix inverse from cache, if available.

cacheSolve <- function(x, ...) {        
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    data <- x$get()
    m <- solve(data,...)
    
    x$setinv(m)
    m
}
