## Caching the Inverse of a Matrix programming asssignment

# The bellow function creates a special "matrix" object that can cache its inverse.
# It contains a function to
# set the value of the matrix -> set 
# get the value of the matrix -> get 
# set the value of the inverse -> setinverse 
# get the value of the inverse -> getinverse 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(m)) {
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}