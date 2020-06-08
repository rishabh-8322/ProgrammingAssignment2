## There are two functions: 
## 1) makeCacheMatrix and 2) cacheSolve to cache the matrix and to find the 
## inverse of the matrix. THe first function caches the matrix to be used later 
## and the second function cacheSolve finds the inverse afresh if the caching is 
## not done or caches the result from the first function if the cache exists.
## functions do

## makeCacheMatrix caches the inverse of the matrix to be used later. 

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


## cacheSolve is used to find the inverse of the matrix and does that afresh or 
##uses the cache depending on whether the cache is empty or not

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
