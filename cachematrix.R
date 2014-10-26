## Put comments here that give an overall description of what your
## functions do
## The functions allow one to create a matrix that stores its
## inverse for future use without having to calculate the inverse
## every time it is required but rather retrieve it from memory if it has
## been calculated before. The set must use the <<- assignement operator to access the matrix 
## in its creation environment - using <- will not work :)


makeCacheMatrix <- function(x = matrix()) {
  ## Creates a cached Matrix with Inversed cache capability
  ## returns a list of all the available function on the matrix
  
  i <- NULL
  set <- function(y) {
    ## Looking for x in enclosing environment for deep assignment
    x <<- y
    ## Same deep assignment; Matrix changed and therefore inverse is nulled
    i <<-  NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  ## The value returned by the function is its list of "public" functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




cacheSolve <- function(x, ...) {
  ## The function return the inverse of x as well as store the
  ## the result for future use
  ## x is a cached Matrix, ... additional parameters 
  ## for solve(inverse function)
  ## See help(solve) for possible values 
  i=x$getInverse()
  if(!is.null(i)){
    ## Inverse has been stored before
    message("getting cached data")
    return(i)
  }
  ## Inverses has not been cached before therefore need to get it
  data=x$get()
  ## Solve the Inverse pass along the parameters ...
  i=solve(data,...)
  ## Need to store the Inverse into the cache
  x$setInverse(i)
  i
}
