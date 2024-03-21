## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
##   write a pair of functions that cache the inverse of a matrix.
##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }        
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set,get = get,
       setInverse = setInverse,
       getInverse = getInverse)    
}

## Write a short comment describing this function

## Write a short comment describing this function
##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed)
##  , then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'     
  m <- x$getInverse()
  if (!is.null(m)){
    message("getting cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}

## Test 1-------
m_matrix$set(matrix(c(2, 3, 1, 1, 1, 0, 4, 0, 3), 3, 3))
m_matrix$get()
     [,1] [,2] [,3]
[1,]    2    1    4
[2,]    3    1    0
[3,]    1    0    3
cacheSolve(m_matrix)
           [,1]       [,2]       [,3]
[1,] -0.4285714  0.4285714  0.5714286
[2,]  1.2857143 -0.2857143 -1.7142857
[3,]  0.1428571 -0.1428571  0.1428571

## Test 2 --------
m_matrix$set(matrix(c(3, 1, 5, 2), 2, 2))
m_matrix$get()
     [,1] [,2]
[1,]    3    5
[2,]    1    2
cacheSolve(m_matrix)
     [,1] [,2]
[1,]    2   -5
[2,]   -1    3
        

