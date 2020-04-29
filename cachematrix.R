## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Author:JguzmanT
##Since the code for initializing a cache vector was given in the instructions
#it is only matter of modifying the variables relating now to the inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvM <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinvM = setinvM,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) #Obtains the inverse of the matrix
  x$setinvM(m)
  m
}

