makeCacheMatrixE <- function(x = matrix()) {
  l <- NULL
  set <- function(y){
    x <<- y
    l <<- NULL
  }
  get <- function() x
  set_inverse<- function(inverse) l <<-inverse
  get_inverse<- function() l
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
  
}

## Write a short comment describing this function
## This other function calculates the inverse of the special matrix created 
## in the previous function. The very first thing it does is to chec if the
## inverse has been already calculated. If that is true, then the function 
## gets the inverse from the cache. If not, then it calculates the inverse 
## of the matrix and sets the value of the inverse in the cache.

cacheSolveE <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  l <- x$get_inverse()
  if (!is.null(l)){
    message("looking for data at cache")
    return (l)
  }
  data <- x$get()
  l <- solve(data, ...)
  x$set_inverse(l)
  l
}