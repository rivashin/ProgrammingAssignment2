## Here we have two functions that allow to inverse a matrix and cache the inversion result for further use.
## Use makeCacheMatrix() function to create "matrix" object that supports caching.
## Use cacheSolve() function to invert the matrix.


##  This function creates a special "matrix" object that can cache its inversion.
## Supported operations:
## set - sets new matrix. This will clear the cached inverted
## get - returns new matrix
## setinverse - caches the inverse matrix
## getinverse - returns the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	get <- function() x

	setinverse <- function(y) inverse <<- y
	
	getinverse <- function() inverse

	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(is.null(inv)) {
		message("solving the matrix...")
		data <- x$get()
		inv <- solve(data, ...)
		x$setinverse(inv)
	}
	inv
}
