## This group of 2 functions allows to invert squared invertible matrices
## and store in cache the result for later use


## The function makeCacheMatrix builds a special data type from a matrix
## The function takes a matrix as input and builds a list of 4 functions 
## 1- A function which stores the input matrix
## 2- A function which outputs the stored variable M (if any)
## 3- A function which stores the inverse of the input in a variable INV
## 4- A function which outputs the stored variable INV (if any)
## The function assumes the input matrix is quare and invertible

makeCacheMatrix <- function(M = matrix()) {

  ## 1- A function which stores the input matrix in variable M and empty variable INV
  INV <- NULL
  set <- function(y) {
    M <<- y
    INV <<- NULL
  }
  
  ## 2- A function which outputs the stored variable M (if any)
  get <- function() M

  ## 3- A function which stores the inverse of the input in a variable INV
  setinverse <- function(inverse) INV <<- inverse
  
  ## 4-A function which outputs the stored variable INV (if any)
  getinverse <- function() INV

  ##Store all 4 functions in a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the special data type output by function makeCacheMatrix
## If the cach is empty, it computes the inverse of the input matrix
## If the cache is full, it returns the cache 

cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'
    
    ## Get the value stored in cache  
    INV <- x$getinverse()
    ## Return it if not null
    if(!is.null(INV)) {
      message("getting cached data")
      return(INV)
    }
    
    ##Get the input matrix
    data <- x$get()
    ## Invert it, assuming the matrix is squared and invertible
    INV <- solve(data, ...)
    ##Store in cache the result
    x$setinverse(INV)
    ##output the result
    INV
}
