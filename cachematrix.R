## So this file contains functions to calculate and cache the inverse of a 
## matrix, the two functions being makeCacheMatrix and cacheSolve
##
## These functions have the following example usage::
##
##                  input <- matrix(runif(3^2),3) ## Generate invertible matrix
##                  cacheMatrix <- makeCacheMatrix(input) ## 
##                  cacheSolve(cacheMatrix)
##

## This generates a simple cacheable version of the matrix supplied, which
## is then used by the cacheSolve function to determine and store the inverse of a 
## matrix supplied 
## 
## The cacheable matrix has 4 functions:
##
## 1. set the value of the original matrix
## 2. get the value of the original matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
        
        invX <- NULL
        set <- function(y) {
                x <<- y
                invX <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invX <<- inv
        getinv <- function() invX
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function gets the inverse of the cacheable matrix supplied, if the inverse of the matrix
## hasn't been calculated then it will calculate the inverse and store it in the cacheable matrix.
## If the cacheable matrix already contains an inverse for the matrix, then that cached inverse
## is returned
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Do I have a cached version of the inverse for this matrix?
        invX <- x$getinv()
        if(!is.null(invX)) {
                ## Cached inverse exists so returning cached version
                message("getting cached data")
                return(invX)
        }
        
        ## Cached version doesn't exist, so calculate the inverse
        data <- x$get()
        invX <- solve(data, ...)
        
        ## Populate the cache for subsequent calls
        x$setinv(invX)
        
        ## Now return the result
        invX
}
