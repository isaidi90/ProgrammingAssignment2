#First of all, thank you for the time you're taking for evaluating my work
#As described in the instructions, two functions should be writtent
#The first one "makeCacheMatrix" creates a special "matrix"
#The second one computes the inverse of the special "matrix" returned by makeCacheMatrix.
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachSolve should retrieve the inverse from the cache

## makeCacheMatrix creates a special matrix wich is a list containing a function to :
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the makeCacheMatrix function.
  #If the inverse had already been calculated. If so, it gets the inverse from the cache and skips the computation.*
  #Otherwise, it calculates the inverse of the data and sets the value of the inverse 
  #in the cache via the setminverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data , ...)
  x$setinverse(inv)
  inv
}

