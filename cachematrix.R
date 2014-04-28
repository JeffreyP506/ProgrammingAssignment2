# The first function, makeVector creates a special "matrix", which is really a list
# containing a function to

# set the value of the matrix
# get the value of the matrix
# set the value of the inverse matrix
# get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # variable for caching the inverse matrix
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

# The following function calculates the mean of the special "matrix" created 
# with the above function. However, it first checks to see if the mean has 
# already been calculated. If so, it gets the mean from the cache and skips
# the computation. Otherwise, it calculates the mean of the data and sets the
# value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        ## Return a matrix that is the inverse of 'x'
}
