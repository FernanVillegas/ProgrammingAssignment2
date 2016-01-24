## TAREA 2

## Parte 1

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # Inverse property
  set <- function(y) {  # Set the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x  # Get the matrix
  setinverse <- function(inverse) inv <<- inverse  # Set the inverse of the matrix
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)  # List of the methods
}

## Parte 2

## cacheSolve: 
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse() # Return a matrix inverse of x
  if(!is.null(inv)) { # Returns the inverse if it already exists
    message("getting cached data.")
    return(inv)
  }
  data <- x$get() # Get the matrix from our object
  inv <- solve(data) # Calculate the inverse
  x$setinverse(inv) # Set the inverse to the object
  inv # Return the matrix
}

# This function assumes that the matrix is always invertible
