## Computes the inverse of a matrix! But if the inverse
## has already been calculated, we use the cached result

## Just like in the assignment setup example, the make CacheMatrix function:
# (1) sets the value of the matrix
# (2) gets the value of the matrix
# (3) sets the value of the inverse
# (4) gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve() is the critical function here. It computes the 
# inverse of the matrix. If matrix has already been computed,
# returns a cached result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}


### TESTING ###
## Notes: first cacheSolve() should return inverse, second cacheSolve()
## call should mention that its getting the cached data
#> a = matrix( c(2, 3, 2, 2), nrow=2, ncol=2, byrow = TRUE)
#> test = makeCacheMatrix(a)
#> cacheSolve(test)
#> cacheSolve(test)
