## Functions in this script are for creating an object that stores a matrix and
## caches its inverse.

## Creates a list with setter and getter methods that set and get a matrix and
## methods for setting and getting the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Finds the inverse of the matrix that the methods of the list created in makeCacheMatrix 
## handle.
## Caches the inverse if the inverse has not already been calculated. Checks the
## existence of the cached inverse before calculating it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
