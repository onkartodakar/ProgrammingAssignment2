## Put comments here that give an overall description of what your
## functions do
##Week3 Assignment - Functions to cache inverse of a matrix to avoid redoing costly matrix inversion operation

## Write a short comment describing this function
# Function to create special CacheMatrix which caches value of its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<-inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("Gettng cached data...")
    return(inv)
  }
  data <- x$get()
  inv <-solve(data, ...)
  x$setInverse(inv)
  inv
}
