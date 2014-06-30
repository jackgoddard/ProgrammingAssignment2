## Put comments here that give an overall description of what your
## functions do

## A function that creates a special 'matrix' object that can cache its inverse.
## Function:
## 1. Sets the value of the matrix
## 2. Gets the value of the matrix
## 3. Sets the values of the inverse
## 4. Gets the values of the inverse
## 5. Returns a list with the values for steps 1.- 4. 

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x   <<- y
            inv <<- NULL
      }
      
      get <- function() x

      setinv <- function(inv) { inv <<- solve(x) }
      
      getinv <- function() { inv }
      
      list( set = set, get = get, setinv = setinv, getinv = getinv )      
}


## This function computes the inverse of the special matrix, or if it is already cached it returnes the cached value
## The function does:
##  1. Get values of x$getinv()
##  2. Test if x$getinv() has a value set yet
##  3. If number is set in cache, return this value
##  4. If not, get the matrix, calculate the inverse, and set in cache
##  5. Return the inverse of the matrix

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if( !is.null(inv) ){
            message("getting cached data")
            return(inv)            
      }
      
      data <- x$get()
      inv  <- solve(data)
      x$setinv(inv)
      inv
      
}
