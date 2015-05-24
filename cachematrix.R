## These two functions try to find if the inverse of the matrix stored in the cache.
## If yes, do not calculate again.
## If no, calculate the matrix inverse and set the matrix inverse in the cache.

## makeCacheMatrix creates a special "matrix", which is really a list contianing a function to
## set the value of the vector
## get the value of the vector
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function()
  setinverse <- function(solve)
  getinverse <- function()    
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve calculate the inverse of the special "matrix" created with above function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from tnhe cache and skips the computation.
## Otherwise, it calculate the inverse of the data and sets the inverse of the matrix in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}