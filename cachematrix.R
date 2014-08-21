###############################################################################
# Homework Assignment #2
#
#   Matrix inversion is usually a costly computation and their may be some 
#   benefit to caching the inverse of a matrix rather than computing it 
#   repeatedly. 
#
#   The assignment is to write a pair of functions that cache the 
#   inverse of a matrix.
###############################################################################

###############################################################################
# This function creates a special "matrix" object that can cache its inverse.
###############################################################################
makeCacheMatrix <- function(x) {
	inv <- NULL # set inverse to NULL until setinv is called
	set <- function(y) { # defines the matrix value
		x <<- y
		inv <<- NULL # setting inverse value to NULL since matrix has changed
	}
	get <- function() x # simply returns the value of the matrix
	setinv <- function(invMatrix) inv <<- invMatrix # assigns a value for inverse
	getinv <- function() inv # returns the inverse value
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv) # list points to the four functions defined internally
}

###############################################################################
# This function computes the inverse of the special "matrix" returned by 
#   makeCacheMatrix above. If the inverse has already been calculated 
#   (and the matrix has not changed), then the cachesolve should retrieve 
#   the inverse from the cache.
###############################################################################
cacheSolve <- function(x, ...) {
	inv <- x$getinv() # get the current value for the inverse
	# if inverse is already defined, return it
	if(!is.null(inv)) { 
		message("getting cached data")
		return(inv)
	}
	# if inverse is not yet defined, compute it and set it
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}