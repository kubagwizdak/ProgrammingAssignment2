
## makeCacheMatrix creates a list of 4 functions that do the following:
## set - stores a given matrix
## get - returns the stored matrix
## setInverse - stores the inverse of the stored matrix
## getInverse - returns the stored image 

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) Inv <<- inverse
        getInverse <- function() Inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## cacheSolve checks if the inverse of the matrix returned by makeCacheMatrix 
## is being stored in makeCacheMatrix. If so - it returns it, otherwise 
## cacheSolve computes the image by herself.

cacheSolve <- function(x, ...) {
        Inv <- x$getInverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInverse(Inv)
        Inv
}