## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
    xi = NULL
    set <- function(y) {
      x <<- y
      xi <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) xi <<- inverse
    getinverse <- function() xi
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xi <- x$getinverse()
  if(!is.null(xi)) {
    message("getting cached data")
    return(xi)
  }
  mat.data = x$get()
  xi <- solve(mat.data, ...)
  x$setinverse(xi)
  xi
}
