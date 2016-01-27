## Create special vector, which is really a list containing
## a function to set value of vector, get value of vector, 
## set value of the matrix inverse, get value of matrix inverse.

## We save two variables x and i in the cache. In OOP parlance, 
## they are the "attributes" of our "class" of input matrices. 
## x is the value of the input matrix, i is its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## Initialize our "input matrix object".  In OOP parlance, 
  ## set() is our "constructor function".
  set <- function(y) {
            x <<- y
            i <<- NULL
  }   
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  ## In OOP parlance, these are the "methods" of our "class" of input matrices.
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## The following function calculates the matrix inverse of
## the vector created by the above function, makeCacheMatrix().

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
     message("getting cached data")  
     return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}