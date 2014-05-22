## Put comments here that give an overall description of what your
## functions do
##The 

## Write a short comment describing this function

## This function takes a matrix and makes a special object that contains the list of methods
## As the course instructions state this can cache the matrix invers with the setinverse function
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

##cash solve takes as an argument the special matrix object cretead with makeCacheMatrix and return the inverse
## the inverse is either calculated or is retrieved from a cached version as shown in the !is.null() logic

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        
        x$setinverse(i)
        i
}
