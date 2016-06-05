## `makeCacheMatrix` creates a special matrix that can hold its cached inverse
##
## internal fields
## x : cached matrix
## i : cached inverse
##
## To explore:
## use matrix equality to avoid assignment when matrix is already cached
## https://stat.ethz.ch/pipermail/r-help/2012-June/315408.html?BHT-9c73f427-7e3a-bc46-bc8a-5ff5849f6d30.0

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  list(
    set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

## Return a matrix that is the inverse of 'x'
## If there is a cached inverse return that,
## if not calculate, cache, and return the inverse of the matrix
##
## `x`: is the special matrix made by makeCacheMatrix
##  ex: makeCacheMatrix(matrix(c(3,1,7,5)),2,2)

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  data = x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
