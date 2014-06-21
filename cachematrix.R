
## makeCaheMatrix creates a special "matrix" object that can cache its inverse.
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

inv_mat <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  # get the value of the matrix
  get <- function() x
  
  #set the value of the inverse matrix
  setsolve <- function(solve) inv_mat <<- solve
  
  #get the value of the inverse matrix
  getsolve <- function() inv_mat
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##  cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix 
##  above it first checks to see if the solve has already been calculated. 
##  If so, it gets the solve from the cache and skips the computation. 
##  Otherwise, it calculates the mean of the data and sets the value of the mean in 
##  the cache via the setmean function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

inv_mat <- x$getsolve()
  if(!is.null(inv_mat)) {  # checks to see if the solve has already been calculated
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$setsolve(inv_mat)
  inv_mat
}
