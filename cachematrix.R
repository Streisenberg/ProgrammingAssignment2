makeCacheMatrix <- function(x = matrix()) {
  e <- NULL
  set <- function(y) {
    x <<- y
    e <- NULL
  }
  get <- function() {x}
  setInv <- function(inverse) {e <<- inverse}
  getInv <- function() {e}
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(x, ...) {
  
  e <- x$getInv()
  if(!is.null(e)) {
    message("getting cached data")
    return(e)
  }
  mat <- x$get()
  e <- solve(mat, ...)
  x$setInv(e)
  e
}