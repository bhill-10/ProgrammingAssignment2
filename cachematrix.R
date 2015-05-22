calcInverse <- function (x){

## take an invertible matrix "x", store in cache; if matrix changes, calc new
##inverse, otherwise use cached inverse

## creates list of original matrix and caches values of matrix

  
  
makeCacheMatrix <- function(x = matrix()) {
  m <-NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function () m
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## checks to see if matrix has changed; if so, calculates new inverse, else
##uses cached inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

#call functions on matrix "x"
out <- makeCacheMatrix(x)
cacheSolve(out)
}