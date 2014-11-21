## makeCacheMatrix - Creates a special "matrix" object that can cache its inverse.
## cacheSolve - Accessor function that uses a makeCacheMatrix object to retrieve inverse matrices
## usage - 1. create a makeCacheMatrix object, e.g. mymatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
##       - 2. retrieve inverse of the makeCacheMatrix object e.g. cacheSolve(mymatrix)
##       - 3. to change the matrix either call makeCacheMatrix again or mymatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2))

## gets and sets fields of the makeCacheMatrix object
makeCacheMatrix <- function(x = matrix()) {
      #sets the base matrix
      inverse <- NULL
      set <- function(y) {
        x <<- y
        inverse <<- NULL
      }
      #returns the base matrix
      get <- function() x
      #returns the cached inverse
      getcachedinverse <- function() inverse
      #sets the cached inverse
      setcachedinverse <- function(cachedinv) inverse <<- cachedinv
      #the internal function list of the object
      list(set = set, get = get,
           setcachedinverse = setcachedinverse,
           getcachedinverse = getcachedinverse)
}


## Computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse 
## has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## First, check to see if we have calculated the inverse on the matrix
        ## in question already. If we have, return the cached inverse, if we 
        ## have not, need to compute the inverse, cache the objects, and return it.
        cachedinv <- x$getcachedinverse()
        if(!is.null(cachedinv)){
              return(cachedinv)
        } 
        #If we're here, we didn't have a cached version, so we need to 
        #calculate and store the values in the cache
        data <- x$get()
        cachedinv <- solve(data, ...)
        x$setcachedinverse(cachedinv)
        cachedinv
}
