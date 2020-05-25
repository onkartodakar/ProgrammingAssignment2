## Put comments here that give an overall description of what your
## functions do
##Week3 Assignment - Functions to cache inverse of a matrix to avoid redoing costly matrix inversion operation

## Write a short comment describing this function
# Function to create special CacheMatrix which caches value of its inverse
makeCacheMatrix <- function(x = matrix()) {
  #Inverse set to NULL initially
  inv <- NULL
  set <- function(y){
    x <<- y  ## Set value of matrix whose inverse is to be cached
    inv <<- NULL
  }
  get <- function() x  ## get matrix
  setInverse <- function(inverse) inv <<-inverse  # set inverse
  getInverse <- function() inv # get inverse
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){ ## Return cached if present
    message("Gettng cached data...")
    return(inv)
  }
  data <- x$get()  # Get data
  inv <-solve(data, ...)  # Calculate inverse
  x$setInverse(inv) # Cache inverse
  inv 
}
