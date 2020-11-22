## A family of two functions that combined allow for the creation of a matrix 
# with an inbuilt cache for storing its inverse. 

## makeCacheMatrix takes a matrix and returns a list of four functions that
# allow for the setting and retrieval of both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set_matrix <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get_matrix <- function() x
  get_inverse <- function() inverse
  set_inverse <- function(i){
    inverse <<- i
  }
  list(set_matrix = set_matrix, get_matrix = get_matrix, get_inverse = get_inverse,
       set_inverse = set_inverse)
}


## cacheSolve takes an object created by the makeCacheMatrix function. It checks
# if this matrix has an inverse cached within it. If it does it returns the 
# cache, else it calculates the inverse, stores it within the matrix cache, and
# returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cache <- x$get_inverse()
  if (!is.null(cache)){
    return(cache)
  }
  else {
    matrix <- x$get_matrix()
    message("calculating inverse...")
    inverse <- solve(matrix)
    x$set_inverse(inverse)
  }
  inverse
}
