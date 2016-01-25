## The function - makeCacheMatrix returns list of functions for set and get matrix and its inverse
## The function - cacheSolve takes the list from the first function and calculates and sets its inverse.  
##  If the inverse is already set, the cached value is used


## makeCacheMatrix will create a matrix x, and returns functinos for set,get x and its inverse

makeCacheMatrix <- function(x = matrix()) {

  inverseMatrixCached <- NULL 
  
  set <- function(inputValue = matrix()) {
    x <<- inputValue
    inverseMatrixCached <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(newInverse) {
    inverseMatrixCached <<- newInverse 
    return(inverseMatrixCached)
  }
  
  getInverse  <- function() inverseMatrixCached

  list(set=set, get=get,
       setInverse=setInverse, 
       getInverse=getInverse)
}


## With list returned from first function, this function will first check to see
## if there's already a cached inverse, return it otherwise will solve its inverse and set/return it

cacheSolve <- function(x, ...) { 
  
  ## if there is cached inverse
  inversed <- x$getInverse() 
  
  ##check if there's a cached value AND it's a matrix
  if(!is.null(inversed) && is.matrix(inversed)) { 
    message("Inverse from cached data")
    return(inversed)
  }
  
  ## otherwise get the matrix
  changedMatrix <- x$get()  
  
  ## try to solve the matrix and catch errors and warnings
  inversed <- tryCatch({ 
    solve(changedMatrix)
  }, warning=function(w) {
    message("result may not be proper")
    message(w)
  }, error=function(e) {
    message("error while solving matrix")
    message(e)
  })
  
  ## set new inverse
  message("Setting the value of inverse to:") 
  x$setInverse(inversed)
}

