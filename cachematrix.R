## https://github.com/paaronen/ProgrammingAssignment2
## R Programming Assignment 2: Lexical Scoping--caching the inverse of a matrix


## Assignment
## Write the following functions:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL # result 
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) xinv <<- inverse
  getinverse <- function() xinv
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(4, 0, 0, 4), c(2, 2)))
## > cacheSolve(m)
## [,1] [,2]
## [1,] 0.25 0.00
## [2,] 0.00 0.25
