## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of functions that can be used on a matrix
## s is the cache variable for use in the cacheSolve function
## the set function is used to set the matrix
## the get function retrieves the matrix
## the setSolve sets the inverse (when used as part of cacheSolve only)
## the getSolve retreives the inverse (when used as part of cacheSolve only)

makeCacheMatrix <- function(x = matrix()) {
                s <- NULL
                set <- function(y) {
                        x <<- y
                        s <<- NULL
                }
                get <- function() x
                setSolve <- function(solve) s <<- solve
                getSolve <- function() s
                list(set = set, get = get,
                     setSolve = setSolve,
                     getSolve = getSolve)
}


## cacheSolve uses some of the functions and variables defined in makeCacheMatrix
## s in this function is set to the matrix from outside the function using getSolve()
## if makeCacheMatrix was just called, the it will be NULL, but if cacheSolve has been previously called then 
## it will be the result of the previous call
## s is checked to see if it is NULL, if not then s is returned
## if s is NULL, then date gets the matrix, then the solve (inverse) of the matrix is stored in s
## then this inverse matrix is set into the parent object, then printed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setSolve(s)
        s
}


