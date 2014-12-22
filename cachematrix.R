## Put comments here that give an overall description of what your
## functions do
## 
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly 
## These functions are pair of functions that cache the inverse of a matrix.
##
## Author : manish singh
## Date : 22 Dec 2014
## 
## Following functions are available :
##
##    makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##    cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##				If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.
##
## Below functions assumes that the matrix supplied is always invertible and square matrix.
##
## The function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
##    set the value of the vector
##    get the value of the vector
##    set the inverse matrix
##    get the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {

	inverseMatrix <- NA
	set <- function(givenMatrix) {
			x <<- givenMatrix
			inverseMatrix <<- NA
	}
	get <- function() x
	setinverse <- function(solveOfx) inverseMatrix <<- solveOfx
	getinverse <- function() inverseMatrix
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)

}
## end of function makeCacheMatrix
##
## Compare two matrix and return true if both are equal
##
matrixCompare <- function(x,y) {

	## Return True if matrix are equal in dimension as well as values
	if ( (dim(x) == dim(y)) &&
		all(x == y) ) {
			return(TRUE)
	}
	
	## return false if above condition is not met
	return(FALSE)
}
## end of matrixCompare
##
## The following function calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the given matrix and sets the inverse matrix in the cache via the setinverse function.
##
cacheSolve <- function(x, ...) {

	## Return a matrix that is the inverse of 'x'

	inverseOfx <- x$getinverse()
	if(!is.na(inverseOfx)) {
			message("getting cached data")
			return(inverseOfx)
	}
	data <- x$get()
	inverseOfx <- solve(data, ...)
	x$setinverse(inverseOfx)
	inverseOfx
}
## end of function cacheSolve

