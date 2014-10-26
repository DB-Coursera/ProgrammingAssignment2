## cachematrix.R
##
## Create a matrix object to cache the matrix inverse such that the inverse
## need only be computed once. Once computed, successive calls for inversion
## calls return the cached inverse matrix object

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
   cachedInv <- NULL
   set <- function(y) {
      x <<- y
      cachedInv <<- NULL
   }
   get <- function() x
   setInv <- function(Inv) cachedInv <<- Inv
   getInv <- function() cachedInv
   list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix. 
## Retrieve the matrix inverse from cache if already calculated; otherwise compute.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   ## ... from cache, if available
   matrixInv <- x$getInv()
   if(!is.null(matrixInv)) {
      message("getting cached data")
      return(matrixInv)
   }
   ## ... or compute from the input matrix.
   input <- x$get()
   matrixInv <- solve(input, ...)
   x$setInv(matrixInv)
   matrixInv
}
