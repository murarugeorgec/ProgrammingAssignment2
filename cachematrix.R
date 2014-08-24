### Those implementations are 99% the same as Mr. Roger Peng (those given as
## example). There were only needed some modifications to create the matrix


### This function contain some others functions like:
####	set the matrix
####	get the matrix
####	set the inverse of the matrix
				# if you want another inverse than the real one :)
####	get the inverse
				# first must be computed

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y) {
				x <<- y
				i <<- NULL
		}
		get <- function() x
		setinverse <- function(inverse) i <<- inverse
		getinverse <- function() i
		list(set = set, get = get,
		 	 setinverse = setinverse,
			 getinverse = getinverse)
}


### This function computes the inverse of a matrix
### I used an error handler in case the matrix does not have an inverse
### In this case, I preferred to print a message and assign to "i" "no inverse"
##rather than the default value "NULL"

cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
		i <- x$getinverse()
		if(!is.null(i)) {
				message("getting cached data")
				return(i)
        }
		data <- x$get()
		tryCatch(
			{i <- solve(data, ...)},
			error = function(cond)
				{
				 message("The given matrix is singular")
				 i <<- "No Inverse"
				}
		)
		x$setinverse(i)
		i
}
