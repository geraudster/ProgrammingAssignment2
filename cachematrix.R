## Put comments here that give an overall description of what your
## functions do

## This function creates a list of function given a matrix in argument
## The functions set() and get() are used to set or retrieve the content of the matrix
## The functions setInverse() and getInverse() are used to store the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function (y) {
        # `<<-` assign a value in the parent environment, thus persisting the state of a cacheMatrix
        x <<- y
        inverse <<- NULL
    }
    get <- function () x
    setInverse <- function (i) inverse <<- i
    getInverse <- function () inverse
    list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## This function computes the inverse of a cacheMatrix
## It acts as a proxy: if the inverse has already been calculated, it is returned as is.
## Otherwise the inverse is calculated with the solve() function and stored in the cacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
            message("getting data from cache")
            inverse
        } else {
            message("calculating the inverse")
            matrix <- x$get()
            inverse <- solve(matrix, ...)
            x$setInverse(inverse)
            inverse
        }
}

## Tests
myMatrix <- matrix(rnorm(16),4)
cacheMatrix <- makeCacheMatrix(myMatrix)
cacheSolve(cacheMatrix) # Should display "calculating the inverse"
cacheSolve(cacheMatrix) # Should display "getting data from cache"

myMatrix2 <- matrix(rnorm(16),4)
cacheMatrix2 <- makeCacheMatrix(myMatrix2)
cacheSolve(cacheMatrix2) # Should display "calculating the inverse"
cacheSolve(cacheMatrix2) # Should display "getting data from cache"
cacheSolve(cacheMatrix) # Should display "getting data from cache"

myBigMatrix <- matrix(rnorm(5000*5000), 5000)
cacheBigMatrix <- makeCacheMatrix(myBigMatrix)
result <- cacheSolve(cacheBigMatrix) # Should display "calculating the inverse" and take a long time
result <- cacheSolve(cacheBigMatrix) # Should display "getting data from cache" and be very quick :)
