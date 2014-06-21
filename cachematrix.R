## Put comments here that give an overall description of what your functions do
## The functions below 

## Comment
## The first function allows to give certain properties to an object. Those properties permits the 
## user to associate data to a matrix (the object), to display the data and calculate the inverse
## in addition to display its content.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 
  
  get <- function () x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This second function asserts that the first one has correctly set the properties to the matrix
## and allows the user to check if the data (the inverted matrix) has indeed
## been processed and saves (cach) the result or ouput - here the inverted matrix thus. It further
## displays the good working of the function(s)

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

## Testing
testmatrix <- makeCacheMatrix()
testmatrix$set(matrix( c(2,5.2,3,548,236.25,0.254,7,8,9), 3, 3))
testmatrix$get()
cacheSolve(testmatrix)


