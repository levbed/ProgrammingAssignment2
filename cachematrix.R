## makeCacheMatrix creates the 'matrix' object and keeps four functions, that are used by cacheSolve, to return the meaning of 'matrix' inverse (from cashe or from calculations)

## It creates the matrix and keeps the four functions, that can be used by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
                }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## It either returns the inverse meaning from cache, if it's been calculated ealrlier, or it will calculate it using data and functions from makeCacheMatrix

cacheSolve <- function(x, ...) {
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
