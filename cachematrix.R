## The following functions can be used for getting the 
## inverse of a given matrix from the cache (if available)
## instead of calculating the inverse everytime a matrix is supplied

##The 'makeCacheMatrix()' will get the value of matrix
## and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  s <- NULL
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setmatrix <- function(mat) s <<- mat
  getmatrix <- function() s
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
  
}


## The 'cacheSolve()' will check if the inverse of the
## given matrix is available in the cache.
## If not, it will calculate the inverse using 'solve()'

cacheSolve <- function(x, ...) {
  s <- x$getmatrix()
  if(!is.null(s)){
    message("getting cached matrix")
    return(s)
  }
  dat <- x$get()
  s <- solve(dat, ...)
  x$setmatrix(s)
  s
  ## Return a matrix that is the inverse of 'x'
}
