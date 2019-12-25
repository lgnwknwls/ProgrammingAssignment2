## Inverses matrices and archives results for faster lookup

## function which stores matrix along with cached (if available) inverse
makeCacheMatrix <- function(x = matrix()) {
  inverseOfMat <- NULL
  set <- function(y) {
    x <<- y
    inverseOfMat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseOfMat <<- inverse
  getInverse <- function() inverseOfMat
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Looks up cached matrix inverse or solves if not already cached
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixInverse <- x$getInverse()
  if (!is.null(matrixInverse)) {
    message("Getting cached inverse of matrix.")
    return(matrixInverse)
  }
  data <- x$get()
  matrixInverse <- solve(data, ...)
  x$setInverse(matrixInverse)
  matrixInverse
}