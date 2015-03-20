## Programming Assignment 2 for Karen Gatz

## makeSolve: takes a matrix for a parameter and contains subfunctions for providing the inverse matrix 
## cacheSolve: takes a matrix for a parameter and if the inverse matrix has been cached, returns it or 
##            for first time called, calls the functions in makeSolve to find the inverse matrix

## makeSolve: takes a matrix for the parameter and contains subfunctions which are:
## setmatrix: set the inverse matrix using the solve function 
## getmatrix: get the inverse matrix to pass back to calling function
## list: provides listing of matrix and inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve: takes a matrix for the parameter and calls the
## getmatrix.  If cacheSolve has been called previously, the
## matrix inverse will already be cached in memory and returned
## otherwise, on the first call, cacheSolve will call the get
## function to attain the inverse and cache it into memory

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
  
}