## Put comments here that give an overall description of what your
## functions do


## Creates a special matrix-like object containing
## the matrix passed as argument x, the functions to
## get/set the matrix and to get/set the inverse matrix and
## the cached inversed matrix when computed.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inversematrix) i <<- inversematrix
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Computes the inverse of matrix contained in special
## matrix-like object x create through makeCacheMatrix()
## (assumes x is always invertible) and stores it in x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached matrix")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}

