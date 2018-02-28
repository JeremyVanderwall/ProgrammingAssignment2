## makeCachedMatrix makes an object of type CachedMatrix that has 2 accessor functions
## and two mutator function

## constructor for objects of thype cacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setSolve <- function(solve) m <<- solve
  
  getSolve <- function() m
  
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)

}


## returns inverted maxtrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if (!is.null(m)){
    message("getting cached data")
    return (m)
  }
  
  data <- x$get()
  m <- solve(x)
  x$setSolve(m)
  m
}
