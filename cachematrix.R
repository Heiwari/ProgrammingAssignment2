## Caching the Inverse of a Matrix
## uses solve() to calculate the inverse
## of a matrix and cache its results thanks to
## scoping. 
##

## makeCacheMatrix creates a "Matrix" list
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL

### 1. set the value of the matrix
       set <- function(y) {
	x <<- y
	m <<- NULL
}

### 2. get the value of the matrix
       get <- function() x

### 3. set the value of the inverse of the matrix
       setsolve <- function(solve) m <<- solve

### 4. get the value of the inverse of the matrix
       getsolve <- function() m

# return the list
	list(set = set, get = get,
	setsolve = setsolve,
	getsolve = getsolve)
}

## returns the inverse of the matrix
## if a makeCacheMatrix object is supplied
##
## Pattern: cacheSolve(x, ...) - where 'x' is a
## 'makeCacheMatrix' object
##
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
	m <- x$getsolve() # retrieve from cache

	if(!is.null(m)) { # here it goes
	message("getting cached data")
	return(m)
}

# cache isn't filled yet
	data <- x$get()       # get
	m <- solve(data, ...) # solve
	x$setsolve(m)	      # set
	m		      # return
}
