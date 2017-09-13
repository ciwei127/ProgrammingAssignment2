## Cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
          x <<- y
          I <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) I <<- solve
        getinvert <- function() I
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
     I <- x$getinvert()
    if(!is.null(I)) {
      message("getting cached data")
      return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setinvert(I)
    I
}
