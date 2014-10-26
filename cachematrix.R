## Put comments here that give an overall description of what your
## functions do
## get the matrix and inverse it within the cache

## Write a short comment describing this function
## To set and get the value of the matrix and get and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)    
}


## Write a short comment describing this function
## First to check if the inverse has been cached. If so, skip the calculation; 
## Otherwise it set the inverse in the cache

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
