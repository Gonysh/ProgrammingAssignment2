## The function makeCacheMatrix creates an object that allow to set and get a matrix 
## and a second parameter that is ment to be the inverse of the matrix, that is calculated by
## the second function: cacheSolve.

## makeCacheMatrix creates an object that allow to set and get a matrix and a second parameter that is ment to be the inverse of the matrix

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

## cacheSolve checks the input object for an in verse matrix. If it finds it' it returns it.
## If not, it cakcuates it and stores it in the object's environment.

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
