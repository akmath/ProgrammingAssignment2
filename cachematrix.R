## Following functions perform matrix inversion. 
## Since It is a costly operation below functions caches the results 
## to avoid repeated computation of same values

## makeCacheMatrix function creates a special "matrix" object 
## which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #sets the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## get the matrix 
  get <- function() x
    
  ## sets the inverse
  setinv <- function(solve) inv <<- solve
  
  ## get the inverse
  getinv <- function() inv
  
  ## returns the list
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed),
## then this method should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
    i <- x$getinv()
    
    ## check if inverse is already computed.
    ## if yes, it will return from cache
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## get the matrix from special matrix object
    data <- x$get()
    
    ## calcuate the inverse
    i <- solve(data, ...)
    
    ## set the inverse to special matrix object
    x$setinv(i)
    
    ## returns the inverse
    i
}