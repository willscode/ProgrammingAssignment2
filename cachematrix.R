## Put comments here that give an overall description of what your
## functions do
##The 'makeCacheMatrix() function makes an object
## that can store and fetch a matrix and its inverse
##the cacheSolve function takes the result of 'makeCacheMatrix()' as
## its argument. If makeCacheMatrix has never had its matrix solved, 
## cacheSolve can solve it, then store the result in the makeCacheMatrix object
##so that subsequent calls on the same object can simply return the previously computed
## solution

## Write a short comment describing this function
## Uses the list function to return an object containing a matrix, and getter and setter
## functions for the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve #the function for inverting a matrix
  getinv <- function() inv
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
##check whether the previously computed solution is null
##if it is null, compute it. Otherwise, returns its cached value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  mat <- x$get() #get the matrix originally passed to x
  inv<- solve(mat, ...)
  x$setinv(inv)
  inv
}
