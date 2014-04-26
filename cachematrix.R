

## makeCacheMatrix creates an object containing the actual matrix, and a cache value of its inverse
##                 plus the methods to access and set the values of the matrix
##                 and the methods to access and set the cache value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  ##Intialize the value of the Inverse Cached value
  I<-NULL
  
  ## Defines the method to set the value of the matrix to that provided as parameter
  set <- function(y) {
    x <<- y
    ##Being a complete new matrix, previously stored cache value is invalid
    I <<- NULL
  }
  
  ## Defines the method to retrieve the actual value of the matrix.
  get <- function() x
  ## Defines the method to set the cached value of the Inverse of the matrix
  setInverse <- function(Inverse) I <<- Inverse
  ## Defines the method to get the cached value of the inverse of the matrix
  getInverse <- function() I
  
  ## Return a list object containing tha named methods:
  ##       set,get,setInverse,getInverse
  ## Internal data is not made public
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve(x) evaluates the inverse of the cachematrix x if it has not previously calculated
##               in the case it has been calculated before it returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Asks object x por for its cached value of the inverse
        I<-x$getInverse()
  
        if(!is.null(I)){
        ## If the returned value is not null it means that it has been calculated previously
        ## so this function returns the cached value
        message("getting cached data")
        return(I)
        }
        ## If the returned value is null it means that it is the first time to be invoked
        ## so we ask the object x its internal matrix value and we store it in variable M
        M<-x$get()
        ## Invoke the function solve with the matrix M and any other parameters passed to cacheSolve()
        I<-solve(M,...)
        ## Store the calculated value in the cache of the object x
        x$setInverse(I)
        ## return the value of I
        I
}
