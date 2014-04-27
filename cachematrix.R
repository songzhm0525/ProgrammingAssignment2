## Below are two functions that are used to create a special object that stores a 
## matrix and caches its inverse.

## This function creates a special "matrix", which is really a list containing a 
## function to 
#  1. set the value of the matrix
#  2. get the value of the matrix
#  3. set the inverse of the matrix
#  4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    invmat <- NULL
    set <- function(y) {
        x <<- y
        invmat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invmat <<- inverse
    getinverse <- function() invmat
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with 
## the above function. However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the inverse in the cache 
## via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invmat <- x$getinverse()
    if(!is.null(invmat)) {
        message("getting cached data... :D")
        return(invmat)
    }
    data <- x$get()
    invmat <-solve(data,...)
    x$setinverse(invmat)
    invmat
}
