## Put comments here that give an overall description of what your
## functions do


## This function makes a list with different functions 
## to retrieve the matrix and its inversed value
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y=matrix()) {
    x <<- y
    j <<- NULL
  }
  get <- function() x
  setinv <- function(inv) j <<- inv
  getinv <- function() j
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function caches the inverted value of the matrix defined in
## the first function

cacheSolve <- function(x, ...) {
  
  j <- x$getinv()
  if(!is.null(j)) {
    message("The value has been already cached :")
    return(j)
  }
  data<- x$get()
  j <- solve(data,...)
  x$setinv(j)
  j
}