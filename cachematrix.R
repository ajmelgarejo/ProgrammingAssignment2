## Put comments here that give an overall description of what your
## functions do
## These functions, given a matrix, compute and cache its inverse, 
## such that if the inverse already exists it can be retrieved without
## being recomputed

## Write a short comment describing this function
## The function makeCacheMatrix defines the matrix and its inverse 
## and a list of functions to get and set a matrix and its inverse, 
## so the inverse can be cached if already existing

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y ## Set x to the new value y
    inv <<- NULL ## Set the inverse matrix as undefined
  }    
  get <- function() x ## return the matrix x
  setinverse <- function(inverse) inv <<- inverse  ## Set the inverse matrix to inverse
  getinverse <- function() inv ## return the inverse function
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse) ##return a list with the defined functions 
}


## Write a short comment describing this function
## This function checks if the inverse of a matrix exists. If so, 
## it returns it. Otherwise it computes it and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse() ## Retrieve the inverse of x
  if (!(is.null(inv))) ## if the inverse is defined
  {
    message("Getting cached data") ## We use the already existing data
    return(inv)  ## return the cached inverse
  }
  my_matrix <- x$get() ## If the inverse does not exist, retrieve the original matrix
  inv <- solve(my_matrix) ## Compute the inverse
  x$setinverse(inv) ## Cache the inverse
  inv ##return the inverse
}
