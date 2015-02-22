## provides methods to create cacheable matrix, store and retrive cached matrix inverse

## creates cacheable matrix 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #set data
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get data
  get <- function() x
  #set inverse of matrix
  setinverse <- function(inverse) m <<- inverse
  #get inverse of matrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  takes cacheable matrix and returns inverse of the matrix. 
##  if inverse is found in cache, returns cached value.
cacheSolve <- function(x, ...) {
      # get inverse from  cache
      m <- x$getinverse()
      # if found in cache return cached value
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      #get data
      data <- x$get()
      #calculate inverse
      m <- solve(data, ...)
      #set inverse
      x$setinverse(m)
      m
}
