## makeCacheMatrix: This function creates a special "matrix" object that can
#cache its inverse. 
#cacheSolve: This function computes the inverse of the
#special "matrix" returned by makeCacheMatrix. If the inverse has already
#been calculated (and the matrix has not changed), then the cachesolve retrieves
#the inverse from the cache.


##makeCacheMatrix:
#This function creates a special matrix that can cache its own inverse.
#The function takes as "x" argument a matrix.
#To access the value of the matrix call x$get()

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      set <- function(y){
        x <<- y
        i <<- NULL
      }
      
      get <- function() x
      setinverse <- function(inverse){
        i <<- inverse
      }
      
      getinverse <- function() i
      
      list(set = set, get=get, 
           setinverse = setinverse,
           getinverse = getinverse)
}

#cacheSolve:
## This function calculates the inverse of the matrix x.
## x must be a special matrix created with makeCacheMatrix.
## x must be invertible
## the function returns the inverse of the matrix x.
## as a side effect, the function chaches in the matrix object the inverse value
## so that it is faster to calculate when you ask for the inverse again.

cacheSolve <- function(x) {
        
  i <- x$getinverse()
  
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
  
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
  
    i
  
}
