## This code is written to solve the assessment of week 3 of the course
## Programming in R for Coursera. The objective is to avoid the calculation 
## of the same element in a function multiple times, in this case it is the
## inverse of a matrix. For this, two functions were performed
## makeCacheMatrix and cacheSolve.
## Can be used directly by applying to a matrix A
## cacheSolve (makeCacheMatrix (A))

## In this first function a "matrix" element is created that can be catched
## later. The << - operator is used

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)


}


## This function calculates the inverse of the matrix using solve, if the value
## was already calculated before it skips it and simply returns the
## value previously saved

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
