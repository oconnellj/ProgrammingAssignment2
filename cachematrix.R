## The file contains 2 functions, makeCacheMatrix() and cacheSolve().
## makeCacheMatrix() returns a matrix object which can store a cached 
## value for the inverse of the matrix. cacheSolve() returns the inverse 
## of a Matrix - if the matrix already contains a cached value for the
## inverse, it returns that cached value; otherwise, it calculates the 
## inverse and stores it in the cached variable. 

## makeCacheMatrix() creates a new matrix object. The matrix is able to
## store a cached value for the inverse of the matrix. The cached value
## is set to NULL by default.
## The function returns a matrix on which the operations set(), get()
## setcache() and getcache() can be performed, and where the variable
## "c" stores the inverse of the matrix, as a cached variable.

makeCacheMatrix <- function(x = matrix()) {
	## When the matrix is initially created, the cached variable 
	## is set to NULL
	c <- NULL

	## Each time that the value of the matrix is reset, the cached 
	## variable is reset to NULL
	set <- function(y) {
		x <<- y
		c <<- NULL
		}

	## The get() function returns the value of the matrix
	get <- function() x

	## Setcache() sets the value of the cached variable. This function is
	## called from the cacheSolve() function below, to set the cached value.
	setcache <- function(cache) c <<- cache

	## Getcache() returns the current value of the cached variable
	getcache <- function() c
	list(set = set, get = get, setcache = setcache, getcache = getcache)
}

## cacheSolve() returns the inverse of a matrix. If the inverse has
## already been calculated and stored as a cached variable, the function
## returns the cached value; otherwise, it calculates the inverse using
## the solve() function and stores the calculated value in a cached
## variable of the matrix.

cacheSolve <- function(x, ...) {
	## Read the current value of the matrix's cache variable
	cache <- x$getcache()

	## If the cache variable is not NULL, return the cached value
	if (!is.null(cache)) {
		message("Return the cached inverse value")
		return(cache)
		}

	## Otherwise, use the solve() function to calculate the inverse of x
	data <- x$get()
	cache <- solve(data, ...)

	## Having calculated the inverse, set the cached variable to 
	## this value, using the setcache() function above, and return this 
	## newly calculated result
	x$setcache(cache)
	return(cache)	
}
