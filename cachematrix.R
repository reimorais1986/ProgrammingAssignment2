## Put comments here that give an overall description of what your
## functions do

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL # this is where the result of inversion is stored
  set <- function(y) {
    x <<- y
    xinv <<- NULL 
  }
  
  get <- function() x # return the input matrix
  setInv <- function(inv) xinv <<- inv # set the inversed matrix
  getInv <- function() xinv # return the inversed matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(x, ...) {
  invFunc <- x$getInv()
  if(!is.null(invFunc)) {
    message("getting cached data")
    return(invFunc)
  }
  data <- x$get()
  invFunc <- solve(data, ...)
  x$setInv(invFunc)
  invFunc
}
##### Test
## generate a random square, non-singular matrix

#test <- matrix(c(2,2,3,5),2,2)

## generate the makeCacheMatrix object with this matrix

#testCached <- makeCacheMatrix(test)

## from now on calculate or retrieve calculated inversion using the cacheSolve function

#testInv <- cacheSolve(testCached)
#testInv
