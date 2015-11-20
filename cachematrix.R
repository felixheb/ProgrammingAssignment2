## makeCacheMatrix(x = matrix)  creates a matrix object 
## that can cache its inverse. x must be an invertible matrix.
## cacheSolve(x, ...) calculate the inverse

## makeCacheMatrix(x = matrix): This function creates a special "matrix" object 
## that can cache its inverse. x must be an invertible matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # inverse
  
  set <- function(y) { # sets new data and removes the calculated inverse
    x <<- y
    i <<- NULL
  }
  get <- function() # get the matrix
    x
  setinverse <- function(inv) # set the inverse
    i <<- inv
  getinverse <- function() # get the inverse
    i
  
  # return a special "vector", which is a list containing the 4 functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve(x, ...): calculates the inverse of x. It directly returns the inverse
## if it has been calculated before (cache) or calculates it
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  # if inverse has been calculated befor, return the cached value
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # otherwise calculate the inverse and store
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}