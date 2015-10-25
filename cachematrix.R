## This file contains two functions. A short description on their functionalities:
## 1) makeCacheMatrix: Create a special "matrix" object that can cache its inverse
## 2) cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated, the the cachesolve should retrieve the inverse from the cache 

## This function creates a matrix, containing of four elements:
#1.set the value of the matrix
#2. Get the value of hte matrix
#3. Set the value of the inverse
#4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  #Set the value of the matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Get the value of the matrix
  get <- function() x
  #Set the value of the inverse
  setinverse <- function(solve) m <<- inverse
  
  #Get the value of the inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Cashe calculates the inverse of the matrix created with the above function
## However, it first checks if the inverse already has been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the mean of the data
## and sets the value of the inverse via the setinverse function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #solve(x)
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
