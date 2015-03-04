# Author: Al Pivonka
# Date:03-03-2015

# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix
# rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
# Your assignment is to write a pair of functions that cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse
# the matrix supplied must be a square matrix for solve to work or an error is thrown
makeCacheMatrix <- function(theMatrix = matrix()) {
  #Null out the inverse value
  inverse<-NULL 

  #apply the solve to theMatrix and set the inverse value
  inverse<-solve(theMatrix) 

  #the Function get which returns the original matrix
  get <- function() theMatrix
  
  #the Function getInverse which returns the inverse
  getInverse <- function() inverse
  
  #the Function setInverse which takes a new inverse value and assigns it to the inverse
  # this should be a square matrix
  setInverse <- function(newInverse = matix()){
      #Since we are able to set the inverse we have to get back to theMatrix
      #This will also check for a square matrix
      theMatrix<<-solve(inverse)
      #set the inverse
      inverse <<- newInverse
  } 
  list(setInverse = setInverse, getInverse = getInverse,
      get = get)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  #try to obtain the inverse of x from x
  inverse <- x$getInverse()
  # if inverse is not null return it from x
  if(!is.null(inverse)) {
    message("getting cached data")
    ## Return a matrix that is the inverse of 'x'
    return(inverse)
  }
  else{  #create the inverse and set x's inverse to the result of solve of x
    inverse$setInverse(solve(x))
    ## Return a matrix that is the inverse of 'x'
    return inverse$getInverse()
  }
}
