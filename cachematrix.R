##Caching the inverse of a Matrix

## Function"makeCacheMatrix" creates a special "matrix" object can cache its inverse.
## And return a list which contains 4 functions:set(),get(),setinverse(),getinverse()

makeCacheMatrix <- function(x = matrix()) {
  if(!class(x)=="matrix"){
    message("ERROR! please enter an invertible matrix.")
  }
  
    s <- NULL
    set <- function(y){
      x <<- y
      s <-NULL
    }
    get<-function()x
    setinverse<-function(solve)s <<- solve
    getinverse <- function()s
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special"matrix" returned by makeCachMatrix
## above.If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

## assuming the matrix supplied is always invertible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    s <- x$getinverse()
      if(!is.null(s)){
        message("getting cached data")
        return(s)
      }
    
      data <- x$get()
      s <- solve(data, ...)
      x$setinverse(s)
      s
}
