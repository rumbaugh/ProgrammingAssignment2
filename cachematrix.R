makeCacheMatrix <- function(m = matrix()) {
		## Creates a special matrix object m
		## with four attributes:
		##  set - set the matrix m
		##  get - retrieve the matrix m
		##  setinv - set the inverse of m
		##  getinv - get the inverse of m
		inv <- NULL
		set <- function(newm) {
		    m <<- newm
		    inv <<- NULL
		}
		get <- function() m
		setinv <- function(solve) inv <<- solve
		getinv <- function() inv
		list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(m, ...) {
	   ## Returns a matrix that is the inverse of input m
	   ## If the inverse has already been calculated, 
	   ## retrieve it from cache
	   inv <- m$getinv()
	   if(!is.null(inv)) {
	       message("Getting cached inverse")
	       return(inv)
	   }
	   data <- m$get()
	   inv <- solve(data, ...)
	   m$setinv(inv)
	   inv
}