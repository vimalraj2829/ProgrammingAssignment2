## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. The assignment is to write a pair 
## of functions that cache the inverse of a matrix.

## makeCacheMatrix function generates a list containing functions to perform the following operations
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(mat) {
    x <<- mat
    invert <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invert <<- inverse
  getinverse <- function() invert
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The function makeCacheMatrix will return the inverse of the matrix. It first looks It first checks
## for the exitence of an already commputed inverse. If present,it the retieves the inverted matrix.
## If absent, the function computes the inverse of the matrix and it computes the inverse, and sets
## in the cache

## This function works assuming that the matrix is always invertible 
cacheSolve <- function(x, ...) {
  invert <- x$getinverse()
  if(!is.null(invert)) {
    message("Fetching cached Matrix")
    return(invert)
  }
  data <- x$get()
  invert <- solve(data)
  x$setinverse(invert)
  invert
}
