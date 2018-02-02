## This is my submission to the Programming Assignment 2 for R Programming Course on Coursera - Week 3.
## The following code contains two functions, whose purpose is to cache a potentially time-consuming computation, namely calculating the inverse of a matrix.
## Creating such functions is a way to present the leverage of the lexical scooping of R.

## The first function is called makeCacheMatrix. This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The second function, called cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated and the matrix has not changed, then the cacheSolve retreives the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

#Enjoy :)
#Thank you for your attention! 

