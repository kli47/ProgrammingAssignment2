## Overview of makeCacheMatrix()
## makeCacheMatrix() builds a set of functions set(), get(), setinverse(), 
## and getinverse() and returns them within a list to the parent environment
## also creates two objects x and i

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	## makeCacheMatrix() first initializes two objects, x and i
	## x is an argument in the function (an empty matrix)
	## i is set to NULL initializing it as an object 
	## within the makeCacheMatrix() environment 
	## later to be used in the function
	
	## multiple functions are defined in the makeCacheMatrix() environment
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	## set() takes y and assigns it's value to x in the parent environment
	## i.e., makeCacheMatrix() environment
	## set() also assigns the value of NULL to i in the parent environment
	## this clears any value of i that had be previously cached prior to
	## executing function set(), i.e., by cacheSolve()
	
	get <- function() x
	## get() retrieves the value of x from the parent environment
	
	setinverse <- function(urooj) i <<- urooj
	## setinverse() assigns the value of i in the parent environment to 
	## the value passed in the argument cow
	
	getinverse <- function () i
	## getinverse() retrieves i from the parent environment
	
	list(set = set, get = get, 
	     setinverse = setinverse, 
	     getinverse = getinverse)
	## list(...) assigns each function to an element within a list returning 
	## it to the parent environment. each function is now named which
	## allows us to use the $ extract operate to access the functions
}

## Overview of cacheSolve()
## cacheSolve() retrieves and prints the inverse matrix from an 
## object of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
	i <- x$getinverse()
	## sets i to x$getinverse() - see above function makeCacheMatrix() -
	## in theory this object should be a matrix
	
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	## cacheSolve() checks if the value of i is NULL, if the value of i
	## is not equal to NULL this means that i is a valid, cached matrix
	## and then cacheSolve() returns this matrix and the above message 
	## to the parent envrionment

	ourMatrix <- x$get()
	## if !is.null(i) is FALSE, this means that the value of i is NULL 
	## implying there is not a matrix set to i
	## x$get() - see above function makeCacheMatrix() - gets a matrix from
	## the input object
	i <- solve(ourMatrix, ...)
	## takes the inverse of this input object
	x$setinverse(i)
	## sets the inverse in the input object
	i
        ## prints the matrix that is the inverse of "x"
}
