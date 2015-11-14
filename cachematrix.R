## This function will cache the inverse of the matrix given as an argument.
## If the inverse for the given matrix already exists, this function will
## return the cached inverse.  This can save time and processing power for
## complicated matrices.

## This function will cache the inverse of a matrix given as an argument.
## It creates a special "vector," which is really a list containing a
## function to
##     1. set the value of the vector
##     2. get the value of the vector
##     3. set the matrix inverse
##     4. get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setinverse <- function(solve) m <<- solve
       getinverse <- function() m
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## The following function calculates the mean of the special "vector"
## created in makeCacheMatrix function above.  First, though, it
## checks to see if the inverse matrix has already been created.
## If so, it gets the inversion from the cache and skips the the
## computation.  Otherwise, it calculates the inverse of the matrix
## and sets the inverse matrix in the cache via the setinverse 
## function.

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
