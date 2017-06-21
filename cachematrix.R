## Put comments here that give an overall description of what your
## functions do
## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly .

## Write a short comment describing this function
## The following function is used to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <-NULL
  set<-function(y){
    x<<- y
    invMatrix<<-NULL
  }
  get<- function() x
  setInverseMat <- function(inv)  invMatrix <<- inv
  getInverseMat <- function() invMatrix
  list(set = set, get=get,
       setInverseMat = setInverseMat,
       getInverseMat = getInverseMat
  )
}

## Write a short comment describing this function
## The following function first checks if the inverse matrix has already been computed. 
## If so, it returns the inverse without going through the computation. 
## Otherwise, it computes the inverse, sets the cache and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverseMat()
  if(!is.null(invMatrix)){
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix <- solve(data)
  x$setInverseMat(invMatrix)
  invMatrix
}

