## makeCacheMatrix
## input: a matrix
##return value: a list containing functions

##cacheSolve
##input: a list containing functions
##output: inverse of the matrix, that the user passed to makeCacheMatrix

## The user passes a matrix to makeCacheMatrix,
## varibale x stores the currently passed matrix, varible inv stores the inverse of the current matrix
## set() assigns the passed matrix to the variable x and sets the inv to Null
## get() simply returns the current matrix
## setInv() takes in the inverse of the current matrix and sets it to the variable inv
## getInv() fetches the value stored in inv
## finally all the above functions are returned in the form of a list
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


## the function list returned by makeCacheMatrix()
## it first sets the value of inv, if the inv is null that is, the inv hasn't been calculated
## it calculates the inverse (inv <- solve(mat, ...)) and then returns the inverse 
## if the inv is not null that is, it has already been calculated it returns the cached value
## thus avoiding the recalculation if the original matrix has not changed

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
}
