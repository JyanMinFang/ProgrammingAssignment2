## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix manage a set of get/set operations on the matrix and its inverse
## cacheSolve takes a makeCacheMatrix object and returns the inverse 
## of the matrix in the makeCacheMatrix object

## Write a short comment describing this function

## makeCacheMatrix manage a set of get/set operations on the matrix and its inverse
## both the matrix (x) and inverse of it (i) are defined in makeCacheMatrix functoin
## set function set the matrix (x) to a new value (y) and clear the inverse matrix (i)
## set function use the <<- operator, so x and i inside the set function refer to the
## x and i in the makeCacheMatrix function.
## get function return the matrix x. x is a free variable inside the get function,
## and r search rule will find it in the makeCacheMatrix function.
## setinverse function set the inverse matrix, again it use <<- operator to set
## the variable i in the makeCacheMatrix function
## getinverse function return the inverse the matrix, which is a free variable in getinverse
## function and search rule will find it in the makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) { i <<- inverse }
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## cacheSolve return the inverse matrix of the makeCacheMatrix
## it only call solve function to calculate the inverse matrix, if a new matrix is set.
## cacheSolve works by first call makeCacheMatrix object's getinverse() function,
## if getinverse function return a non-null matrix, cacheSolve return the inverse matrix
## if getinverse function return a null matrix, cacheSolve call the get function to get the matrix,
## then call the solve to obtain the inverse matrix and call setinverse to store the calculated
## inverse matrix, before returning the inverse matrix. by calling the setinverse, any subsequent
## cacheSolve calls return the inverse matrix without calling solve again, until a new matrix is set
## via set function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
