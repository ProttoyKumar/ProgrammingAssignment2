## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#Closures get their name because they enclose the environment of the parent function and can access all its variables.
#Closures are useful for making function factories, and are one way to manage mutable state in R.

#It allows us to have two levels of parameters: a parent level that controls operation and a child level that does the work. 
#makeCacheMatrix(), uses this idea to store x and inv in the enclosing environment of the set, get, setInverse, getInverse functions. That means the environment within which they were defined, i.e., the environment created by the makeCacheMatrix().

#makeCacheMatrix(): This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
