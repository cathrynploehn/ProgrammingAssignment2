## Create a matrix object with a cached inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Initial value for the inverse is NULL
  
  set <- function(z) {
    x <<- z
    inv <<- NULL # A new matrix value will reset the inverse value
  }
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  # Create list object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return current inverse value or cache a new inverse value for a matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() # Get inverse value of the function
  
  # If inverse is cached, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  ## Calculate and return inverse of 'x'
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
