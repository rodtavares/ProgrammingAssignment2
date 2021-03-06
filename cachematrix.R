## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # initialize inverted matrix as NULL
        set <- function (z) {
          x <<- z
          inv <<- NULL # when matrix changes, set inverted matrix to NULL
          }
        get <-  function () x
        setinv <- function (inverted) inv <<- inverted
        getinv <- function () inv
        list(set=set, get=get,setinv=setinv, getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if (!is.null(inv)) {
        message("Getting cached inverted matrix")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
