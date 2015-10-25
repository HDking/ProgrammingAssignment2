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
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #Get the value of the matrix
  get <- function() x
  #Set the value of the inverse
  setInverse <- function(solve) i <<- inverse
  
  #Get the value of the inverse
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse
       )
}


## Cashe calculates the inverse of the matrix created with the above function
## However, it first checks if the inverse already has been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it calculates the mean of the data
## and sets the value of the inverse via the setinverse function. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
