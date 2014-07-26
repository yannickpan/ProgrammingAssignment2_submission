## Put comments here that give an overall description of what your
## functions do
## The first function will set the value, get the value of a matrix,
## and set, get the  value of matrix
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL;
  set <- function(y){
    x <<- y;
    m <<- NULL;
  }
  get <- function() x
  setinverse <- function(inverse) m<<-inverse
  getinverse <- function()m
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## the function will first try to get the inverse from function
## and then if it is not in the cache, it will compute the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
