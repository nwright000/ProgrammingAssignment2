## This .R program consists of two function.  When run in succession, they will take an input matrix and export the inverse matrix of the input using the solve() function.
## Because the solve() function can take considerable time, this set of functions will commit any current result to cache so that if the input to makeCacheMatrix()
## has not changed, the cacheSolve() will simply retrieve it from cache.  Because these are two different function and R utilizes lexical scoping it is necesary to
## for the first function to commit the variables to the global environment using the <<- to accomplish this and have the variables availble to the second function.

## This function sets a series of variables (including four functions and two arguements) and passes them to the global environment.

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setSolve <- function(solve) s <<- solve
      getSolve <- function() s
      list(set = set, get = get,
           setSolve = setSolve,
           getSolve = getSolve)
}


## This function evalutes if the output matrix exists and either retrives the existing matrix from cache in global environment or calculates the output matrix.

cacheSolve <- function(x, ...) {
      s <- x$getSolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setSolve(s)
      s
}