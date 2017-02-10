## Matrix inversion is usually a costly computation and there may be some benefit
## to caching ## the inverse of a matrix rather than compute it repeatedly 
## The following pair of functions cache the inverse of a matrix.
## Computing the inverse of a square matrix can be done with the solve function
## in R. 
## Usage solve(a, b, ...)
## solve(a, b, tol, LINPACK = FALSE, ...)
## Arguments
## a - square numeric or complex matrix containing the coefficients of the linear
## system. Logical matrices are coerced to numeric.
## b - a numeric or complex vector or matrix giving the right-hand side(s) of the
## linear system. 
## If missing, b is taken to be an identity matrix and solve will return the
## inverse of a.
## tol - the tolerance for detecting linear dependencies in the columns of a. 
## For more information type ?solve in Console.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
  ## variable to return the inverse    
  inv <- NULL  
        
  set <-function(y){
    ##  "global" variable assigning using <<-         
        x <<- y        
        inv <<- NULL  
  }  
        
    get <- function() x  
            
    setInverse <- function(inverse)inv <<- inverse  
            
    getInverse <- function() inv  
            
        list(set=set, get = get, setInverse = setInverse, getInverse = getInverse)
   }
      
## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache. 
 
      cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'    
        inv <- x$getInverse()  
        if (!is.null(inv)){    
          message("getting cached data")    
                return(inv)  
        }  
              mat <- x$get()
              ## compute inverse  
              inv <- solve(mat,...)   
              x$setInverse(inv)  
              inv #Return value
      }
