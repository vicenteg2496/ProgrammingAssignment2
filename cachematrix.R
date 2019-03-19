#this function creates a matrix, and then store the inverse in a cache variable
makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inve <<- solve
  getinv <- function() inve
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}
# It caclculates de inverse of makeCacheMatrix, but first it check 
#if the inverse of the matrix has already been calculated, if not it calculates.
cacheSolve <- function(x, ...) {
  inve <- x$getinv()
  if(!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  datos <- x$get()
  m <- solve(datos, ...)
  x$setinv(inve)
  inve
  }
