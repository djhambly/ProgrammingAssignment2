## These functions take a square matrix as an input argument, and then produce an
## inverted version of the matrix for output, using the solve() function.

## Take a user-specified matrix as input, and create a list object comprising
## references to several helper functions that set or retrieve the matrix's
## inverse. The function is to be used in conjunction with cacheSolve(). Input
## should follow the below format/procedure (for example):
## 1.     mat <- matrix(rnorm(9), 3, 3) # produce 3x3 matrix w/ 9 random values
## 2.     mat2 <- makeCacheMatrix(mat) # create special matrix object from input
##                                      matrix
## 3.     The result of (2) can then be pushed to cacheSolve(mat2)

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
             setinverse = setinverse, getinverse = getinverse)
}

## Check cache to see whether the input matrix has changed and if an inverted
## version of the matrix exists; if yes, return the cached version. Otherwise,
## invert the matrix and return it as output.

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
