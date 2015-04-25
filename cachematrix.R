## Create a "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
}
get <- function() x
setmatrix <- function(solve) m <<- solve
getmatrix <- function() m
list(set = set, get = get,
     setmatrix = setmatrix,
     getmatrix = getmatrix)
}


## Return the inverse of the "matrix" makeCacheMatrix just created
## When the inverse has already been calculated then the cacheSolve 
##will retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
}
