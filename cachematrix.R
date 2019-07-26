## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  I <- matrix()
  set <- function(y) {
    x <<- y
    I <<- matrix()
  }
  get <- function() x
  setinv <- function(inv) I <<- inv
  getinv <- function() I
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  I <- x$getinv()
  if(!is.na(I[1][1])) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinv(I)
  I
}
