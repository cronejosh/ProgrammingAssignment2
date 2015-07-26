## Author: Joshua Crone
## Date: July 25, 2015
## Course: Coursera - R Programming
## Title: Assignment #2
##
## This file contains the definitions for two functions. The first, 
## makeCacheMatrix, is a constructor function for an object that can cache
## both a matrix and its inverse. The second function calculates and caches (if 
## necessary) the inverse matrix, then returns it. 
##

## ===========================================================================

## This function takes a matrix as its only arguement and returns 
## an object that stores that matrix and can cache its inverse. Internal to
## this object are two variables: the matrix and its inverse. The object also
## carries four functions that permit interaction with these variables.

makeCacheMatrix <- function(x = matrix()) {
    # Define a variable for the inverse of the matrix.
    inv <- NULL
    
    # This function caches the stored matrix.
    set <- function(y) {
        x <<- y
        inv <<- NULL  
    }
    
    # This function returns the internally cached matrix.
    get <- function() x
    
    # This function caches the inverse matrix, which is passed as an arguement.
    setInverse <- function(inverse) inv <<- inverse
    
    #This function returns the internally cached inverse matrix.
    getInverse <- function() inv
    
    # This returns the constructed object, which is, at its core, a list of 
    # the above functions.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function takes as its first arguement an object created by the 
## makeCacheMatrix function above. This function returns the inverse of
## the matrix stored in the object. If it has already been calculated, the
## cached value is returned, if not, the inverse is calculated, cached, 
## then returned. Additional, named arguements may be passed to the 
## 'solve' function

cacheSolve <- function(x, ...) {
    # Check if there is an inverse matrix already cached, and return that
    # matrix if found.
    inv <- x$getInverse() 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # The below is executed when there is no cached inverse matrix, so one
    ## must be calculated, cached and returned.
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
