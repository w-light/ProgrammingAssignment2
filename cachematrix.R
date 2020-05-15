## makeCacheMatrix returns an object that contains (provides access to) a list of functions (defined within the function)
## for setting and getting the value of the inverse of the matrix x. (It does not actually calculate the inverse.)
## It also retains the value of the input object x and creates a variable m to hold the result of solve(x), 
## which will be supplied later by cacheSolve.
## Importantly, the functions are made available to the parent environment, and the list method enables calling the individual
## functions with the '$' operator. Thus, they can be called later by the cacheSolve function.
## On the first run of makeCacheMatrix with a new x, the object created holds the list of functions
## and the retained value of x and m equal to NULL (because the cacheSolve function has not run yet).

## cacheSolve completes makeCacheMatrix. It checks if a value is already available for m (corresponding to the argument x).
## If m has some value (because it is cached in the object created by makeCacheMatrix), then it
## reports that value, which is the inverse of x. 
## On the first run of cacheSolve for a particular x, however, the value of m will be NULL. In this case
## cachSolve calculates the inverse of the matrix, reports it to the makeCacheMatrix object by calling the 
## setinverse function of makeCacheMatrix (which sends the value to m), and reports m. If x is not changed, 
## subsequent calls to cacheMatrix will report the m that has already been calculated.


## Create a matrix object and a method of setting and getting its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function () m
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Check if the value of the inverse of x exists. If it does, report that cached value. 
## If not, calculate the inverse of the matrix m and call the function to cache it. Report the inverse.

cacheSolve <- function(x, ...) {
        
  m <- x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
