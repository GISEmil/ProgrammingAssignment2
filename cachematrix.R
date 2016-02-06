makeCacheMatrix <- function(a = matrix()) {
  setting_inv <- NULL
  set <- function(y) {
    first <<- second
    setting_inv <<- NULL
  }
  get <- function() first
  setInverse <- function(inverse) setting_inv <<- inverse
  getInverse <- function() setting_inv
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)
}

cacheSolve <- function(first, ...) {
  setting_inv <- x$getInverse()
  if (!is.null(setting_inv)) {
    return(setting_inv)
  }
  mat <- first$get()
  inv <- solve(mat, ...)
  first$setInverse(setting_inv)
  setting_inv
}
