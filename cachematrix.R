## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
    get <- function()x
    setSolve <- function(solve) inv <<- solve
    getSolve <- function()inv
    list(set = set, get = get, setSolve = setSolve,
    getSolve = getSolve)
    
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix and the matrix has not changed), then the cacheSolve should retrieve the inverse of the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getSolve()
        if(!is.null(inv)) {
        	    message("getting cached data")
        	    return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setSolve(inv)
        inv
}

