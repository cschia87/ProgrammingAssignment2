

# makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#  -  set the value of the matrix
#  -  get the value of the matrix
#  -  set the value of inverse of the matrix
#  -  get the value of inverse of the matrix 
makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	set <- function(y){
			x <<- y
			m <<- NULL
	}
	
	get <- function() x
	setInverse <- function(invMatrix) m <<- invMatrix
	getInverse <- function() m
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	
}


## function return the inverse of matrix, 
#   	- get from cache if the inverse matrix already existed (have been computed before)
#	 	- else compute and caching those inverse matrix for use in future 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		m <- x$getInverse()
		if(!is.null(m)){
				message("getting cache data")
				return(m)
			
		}
		data <- x$get()
		m <- solve(data, ...)
		x$setInverse(m)
		m
}
