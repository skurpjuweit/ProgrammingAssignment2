## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than computing it repeatedly. This file contains a pair of functions that cache the inverse of a matrix.
## 1) makeCacheMatrix(): This function creates a special "matrix" object that can cache its inverse.
## 2) cacheSolve() : This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


## MakeCache creates a special object that stores an invertible matrix and caches its inverse.
## The special object is a list containing a set of functions to
## 1. set the value of the matrix (set)
## 2. get the value of the matrix (get)
## 3. set the value of the inverse (setinverse)
## 4. get the value of the inverse (getinverse)
makeCacheMatrix <- function(x = matrix()) {
  
  # i is the variable to store the cached inverse
  i <- NULL
  
  # x is the variable to store the invertible matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inv) i <<- inv
  
  getinverse <- function() i
  
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
  
}


## The cacheSolve function calculates the inverse of a matrix represented as a speicl object created with makeCacheMatrix().
## It checks if the inverse has already been calculated. If yes, it reads the inverse from the cache. If not, it calculates the
## invese and stores the result in the cache using the setinverse function.
cacheSolve <- function(x, ...) {
  
  # read the cached inverse
  i <- x$getinverse()
  
  # if a cached inverse is present, use it as return value
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # if not, calculate the inverse and store it in the cache
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
