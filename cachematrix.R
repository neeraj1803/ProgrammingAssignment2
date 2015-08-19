## Caching the inverse of matrix 
## makeCacheMatrix creates a special "matrix" to
## 1=> set the value of matrix.
## 2=> get the value of matrix.
## 3=> set the value of matrix's inverse.
## 4=> get the value of matrix's inverse.
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
			x <<- y
			m <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) m <<- solve
		getinverse <- function() m
		list(set = set,
			get = get,
			setinverse = setinverse,
			getinverse = getinverse)
}



##cacheSolve calculates the inverse of matrix created with the above function. However, it first checks to see -  
## - if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of matrix and sets the value of the inverse in the cache via the setinverse function.
## This function will return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
