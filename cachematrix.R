#accept a matrix x
#


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(passedMatrix){
    x <<- passedMatrix
    inv <- NULL
  }
  
  get <- function(){
    x
  }
  
  setInv <- function(invVal){
    inv <<-  invVal
  }
  
  getInv <- function(){
    inv
  }
  list(ste = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(is.null(inv) == FALSE){
    message("getting cached value")
    return(inv)
  }
  else{
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInv(inv)
    inv
  }
  ## Return a matrix that is the inverse of 'x'
}
