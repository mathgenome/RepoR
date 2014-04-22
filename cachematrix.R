## These functions are aimed to caching the inverse of a matrix to speed up the inversion process in a loop
## The first function creates a list object with functions to:
## -get the data
## -set the inverse matrix
## -get the inverse
## x is a squared singular matrix

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(get = get, setinv = setinv, getinv = getinv)
}


## The second function first checks if the inverse
## has been calculated for the same data in the last step
## If it has been already calculated, it outputs the
## last result
## If it has not been already calculated, it gets
## the inverse by the solve() function and stores the solution
## x is a makeCacheMatrix object

cacheSolve <- function(x, ...) {inv<-x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	my.matrix<-x$get()
	inv <- solve(my.matrix)
	x$setinv(inv)
	inv
        ## Return a matrix that is the inverse of 'x'
}
