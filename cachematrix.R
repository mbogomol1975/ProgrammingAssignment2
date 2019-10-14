## The makeCacheMatrix function creates an R object that stores a matrix x and its inverse 
## in the parent environment of the makeCacheMatrix function. 
## The cacheSolve function retreives the cached inverse matrix or calculates a new inverse 
## for the R object returned by makeCacheMatrix.

## The makeCacheMatrix function returns an object of type makeCacheMatrix() together with the list of four 
## functions: set, get to create and retrieve matrix x and setinverse, getinverse to set and
## retrieve the inverse of x, m. The function initiallizes the inverse matrix to NULL.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  
  get<-function() x
  
  setinverse<-function(inv)  m<<-inv
  
  getinverse<-function() m
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cashSolve function returns the inverse of x, if it has been cached, via getinverse, 
## or calculates an inverse for new matrix and stores it in m, via setinverse.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
