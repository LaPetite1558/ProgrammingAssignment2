## Put comments here that give an overall description of what your
## functions do

## create matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # set value of matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get value of matrix
  get <- function() x
  
  # set value of inverse matrix
  setInverse <- function(inverse) inv <<- solve(x) #calculate the inverse
  
  # get value of inverse matrix
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(x = matrix(), ...) {
  inv <- x$getInverse()
  
  # if stored inverse matrix is not null, return inv
  if(!is.null(inv)) {
    message("cached inv. matrix found")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
