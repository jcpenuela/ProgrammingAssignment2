## Put comments here that give an overall description of what your
## functions do

## I'm not native english speaker, please, be patient. :-)
## Functions manage a special type created to calculate the inverse
## of a matrix. The special feature is in caching the result of inverse
## instead of calculate it every time the solve is called.
## The use is as follow:
##  1. Create the matrix calling the function makeCacheMatrix, for
##     this example we can use the matrix:
##
##                    | 2  0  1 |
##                    | 1  1 -4 |
##                    | 3  7 -3 |
##
##     > m <- makeCacheMatrix(matrix(c(2,1,3,0,1,7,1,-4,-3),3,3))
## 
##  2. Test the matrix is created:
##
##     > m$get()
##          [,1] [,2] [,3]
##       [1,]    2    0    1
##       [2,]    1    1   -4
##       [3,]    3    7   -3
##
##  3. Calculate the inverse using the cacheSolve function:
##
##     > cacheSolve(m)
##                 [,1]       [,2]        [,3]
##     [1,]  0.46296296  0.1296296 -0.01851852
##     [2,] -0.16666667 -0.1666667  0.16666667
##     [3,]  0.07407407 -0.2592593  0.03703704
##
##
##  4. Verify that a new call to cacheSolve show the same result
##     and display a message indicating that data was taken from cache.
##
##     > cacheSolve(m)
##     getting cached data
##                 [,1]       [,2]        [,3]
##     [1,]  0.46296296  0.1296296 -0.01851852
##     [2,] -0.16666667 -0.1666667  0.16666667
##     [3,]  0.07407407 -0.2592593  0.03703704
##


## class which define the special matrix with inverse cached
## it return a list of functions which are used to access to
## the cached inverse value and the original matrix itself.
makeCacheMatrix <- function(x = matrix()) {
  # Initial value of the inverse, at creation time, is NULL
  i <- NULL
  # function to assign/reassign content of the matrix
  # the function clear the value of the inverse cached (if
  # despite it was cached previously or not)
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # return the matrix
  get <- function() x
  # assign a value to the inverse value of the matrix
  # actually, the inverse is not calculated here but using
  # the solveCache function.
  setinverse <- function(inverse) i <<- inverse
  # return the cached inverse
  getinverse <- function() i
  # return the object (a list of functions) which is our
  # special matrix type
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function solve the matrix, but use the special class 
## makeCacheMatrix, which return the cached inverse
## The function get the cached inverse and, in case it was NULL
## it calculate the inverse and then caches it calling the
## makeCacheMatrix$setinverse() method, so the next time it
## get the inverse, it will obtain the cached data.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  # check if the inverse obtained is NULL (no previously calculated)
  if(!is.null(i)) {
    ## the data is cached. Use it and show a message indicating
    message("getting cached data")
    return(i)
  }
  ## get the original matrix for inverse calculating
  data <- x$get()
  ## get the inverse
  i <- solve(data, ...)
  ## caches it
  x$setinverse(i)
  ## return the fresh calculated inverse
  i
}
