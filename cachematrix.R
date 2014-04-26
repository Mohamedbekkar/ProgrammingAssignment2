## we devlop 2 function in this scriot
## makeCacheMatrix creates a special matrix object, and calculate the inverse
## cacheSolve calculates the inverse of a given matrix.

makeCacheMatrix <- function(x = matrix()) {
  inversx <- NULL
  set <- function(y) {
    x <<- y
    inversx <<- NULL
  }
  get <- function() x
  setinver<- function(inverse) inversx <<-solve(x)
  getinver <- function() inversx
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


## cacheSolve Return a matrix that is the inverse of (x)
## If the matrix inverse has already been calculated in makeCacheMatrix
## it will be recovred
## otherwise it will be calculated on the function cacheSolve
cacheSolve <- function(x, ...) {
        
  inversx <- x$getinverse()
  if (!is.null(inversx)) {
    message("getting the cached inverse matrix")
    return(inversx)
  } else {
    inversx <- solve(x$get())
    x$setinver(inversx)
    return(inversx)
  }

}
