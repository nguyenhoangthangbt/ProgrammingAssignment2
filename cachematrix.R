## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_invMatrix <- function(value) m <<- value
  get_invMatrix <- function() m
  list(set = set, get = get,
       set_invMatrix = set_invMatrix,
       get_invMatrix = get_invMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_invMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- if (nrow(data)==ncol(data)) {solve(data)} else { print("The matrix must be square, please try again!") }
  x$set_invMatrix(m)
  m
}
