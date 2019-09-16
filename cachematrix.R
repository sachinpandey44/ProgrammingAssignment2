## These functions are defined to get and set the inverse of a matrix and cache them. 

## This function takes a matrix and returns a list of operations on the matrix. 
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
                }
        get <- function()x
        setinverse <- function(m) inverse <<- m
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Solves the inverse of a matrix, caches it, and retrives the cached response on subsequent calls. 
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
                }
        data <- x$get()
        print(data)
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
