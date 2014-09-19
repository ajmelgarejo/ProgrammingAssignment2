## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse ## Retrieve the inverse of x
  matrix <- x$get
  if (!is.null(inv) & matrix == x) ## if the inverse is defined and the matrix has not changed
  {
    message("Getting cached data") ## We use the already existing data
    return(inv)  ## return the cached inverse
  }
  inv <- solve(x) ## In case the inverse does not exist, compute it
  x$setinverse(inv) ## Cache the inverse
  x$set(matrix) ## Set the value of the matrix we solved
  inv ##return the inverse
}
