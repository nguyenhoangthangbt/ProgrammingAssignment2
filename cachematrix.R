## This script contains two functions that work together to cache the inverse of a matrix.
## The first function, makeCacheMatrix, creates a special "matrix" object that can store its inverse.
## The second function, cacheSolve, calculates the inverse of this matrix and caches it,
## or retrieves the cached inverse if it has already been calculated.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize 'm' to NULL. This variable will store the cached inverse of the matrix.
  m <- NULL
  
  ## Define a function 'set' to set the matrix data.
  set <- function(y) {
    ## Assign the input matrix 'y' to the variable 'x' in the parent environment.
    x <<- y
    ## Reset the cached inverse 'm' to NULL whenever the matrix data is changed.
    m <<- NULL
  }
  
  ## Define a function 'get' to get the matrix data.
  get <- function() x
  
  ## Define a function 'set_invMatrix' to set the cached inverse.
  set_invMatrix <- function(value) m <<- value
  
  ## Define a function 'get_invMatrix' to get the cached inverse.
  get_invMatrix <- function() m
  
  ## Return a list of these functions. This list is the special "matrix" object
  ## that allows interaction with the matrix data and its cached inverse.
  list(set = set, get = get,
       set_invMatrix = set_invMatrix,
       get_invMatrix = get_invMatrix)
}


## This function calculates the inverse of the special "matrix" created by makeCacheMatrix.
## It first checks if the inverse has already been calculated and cached.
## If it has, it retrieves the cached inverse; otherwise, it calculates the inverse
## and caches it before returning it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Attempt to get the cached inverse from the special matrix object 'x'.
  m <- x$get_invMatrix()
  
  ## Check if the cached inverse 'm' is not NULL (i.e., it exists).
  if(!is.null(m)) {
    ## If the inverse is cached, print a message indicating this.
    message("getting cached data")
    ## Return the cached inverse.
    return(m)
  }
  
  ## If the inverse is not cached, get the original matrix data from the object 'x'.
  data <- x$get()
  
  ## Calculate the inverse of the matrix.
  ## First, check if the matrix is square (number of rows equals number of columns).
  m <- if (nrow(data) == ncol(data)) {
    ## If it's square, calculate the inverse using the 'solve' function.
    solve(data)
  } else {
    ## If it's not square, print an error message as inverse is not defined for non-square matrices.
    print("The matrix must be square, please try again!")
    ## Return NULL or handle appropriately (here, the print happens, and 'm' would be assigned the print result which isn't ideal, but matches the original logic structure).
    ## A better approach might be to stop the function here.
    ## For consistency with the original code's flow:
    NULL # Assign NULL if not square to match original structure intent
  }
  
  ## Check if m is not NULL (meaning inverse was calculated successfully for a square matrix).
  if (!is.null(m)) {
    ## Cache the calculated inverse 'm' in the special matrix object 'x'.
    x$set_invMatrix(m)
  } else {
    ## If m is NULL (matrix was not square), stop the function or return gracefully.
    ## Based on the original structure, the print message already happened. We can just return NULL or the message result.
    ## Let's return NULL as inverse wasn't calculated.
    return(NULL)
  }
  
  ## Return the calculated inverse 'm'.
  m
}