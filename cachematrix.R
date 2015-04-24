## Creates a special type of matrix which caches the time consuming operation of inverting a matrix.

## makeCacheMatrix is called to generate the special matrix type
## mat, inv are used to store the matrix and inverted matrix
## get, getinv, set, setinv can be used to get/set the matrix and inverted matrix data


makeCacheMatrix <- function(mat = matrix()) {

	inv <<- NULL
	
	get <- function() mat
	getinv <- function () inv
	set <- function (y) {
		mat <<- y
		inv <<- NULL
	}
	setinv <- function (inverse) inv <<- inverse
	list(get = get, getinv = getinv, setinv = setinv)
}


## cacheSolve returns the inverted matrix. If the inverted matrix has previously been cached it reads the value directly, if not it calls the solve function and caches result

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if (!is.null(inv)){
		message ("getting cached data")
		return (inv)
	}
	mat <- x$get()
	inv <- solve(mat)
	x$setinv(inv)
	inv
		
        ## Return a matrix that is the inverse of 'x'
}
