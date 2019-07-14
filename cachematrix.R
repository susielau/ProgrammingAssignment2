## Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	#initialize m
	m <- NULL
	
	#setter for matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	#getter for matrix
	get <- function() x
	
	#setter for inverse of matrix
	setinverse <- function(inverse) m <<- inverse
	
	#getter for inverse of matrix
	getinverse <- function() m
	
	#list of functions available
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
        	message("getting cached inverse")
        	return(m)
        }
        
        data <- x$get()
        
        # Obtain the inverse
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
