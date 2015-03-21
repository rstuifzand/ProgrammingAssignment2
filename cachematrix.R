
## Function that gives back cached data or stores it in the global environment
makeCacheMatrix <- function(x = numeric()) {

  ## empty inverse variable
  i <- NULL
  
  ## set matrix to cache the data
    set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## get the matrix
  get <- function() {
    x
  }
  
  ## set the inverse variable
  setinverse <- function(inverse) i <<- inverse
  
  ## get the inverse variable
  getinverse <- function() i
    
  # creates a list to store the four functions
  list(set = set,
       get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function that calls the stored inverse, or creates it and than sets the inverse 
cacheSolve <- function(x, ...) {

  ## gets the inverse
  i <- x$getinverse()
  
  ## If there is a inverse found than you return a message and the inverse value
  if(!is.null(i)) {
    
    message("getting cached data")
    return(i)
    
  }
  
  ## if inverse is not found than get the data matrix
  data <- x$get()
  
  ## creates the inverse of the matrix with the generic function solve
  i <- solve(data, ...)
  
  ## set the inverse
  x$setinverse(i) 
  
  i  

}
