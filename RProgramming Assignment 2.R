## Here we want to take a self-defined matrix and get its inverse, i.e., the inverse
## of A being A^-1. Once complete, you can ensure that your answer is correct by
## multiplying the matrix (self-defined) by its inverse to see that the answer
## provided by your program is identical to your calculation.

## This function will take the inverse of your self-defined matrix
## and give you its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}
R <- matrix(c(4, 2, 7, 6), 2, 2)
R1 <- makeCacheMatrix(R)
cacheSolve(R1)

## This function calculates the inverse of the function created above, but first
## checks its cache to ensure the inverse already exists. If so, it will simply
## provide the answer. If not, then it will provide you the inverse of the matrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
cacheSolve(R1)
