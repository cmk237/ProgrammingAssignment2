## Put comments here that give an overall description of what your
## functions do
#makeCacheMatrix makes a list that is able to set and store values for a matrix and its inverse
#cacheSolve can return the inverse or calculate it if it does not exist yet

##create a special matrix with get and set methods for itself and inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<-inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("retuning the stored data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
  }
        
