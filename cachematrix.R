#makeCacheMatrix: This function creates a special “matrix” object that can cache its inverse.

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

#cacheSolve: This function computes the inverse of the special “matrix” returned by
#makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
#then the cacheSolve should retrieve the inverse from the cache.

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
