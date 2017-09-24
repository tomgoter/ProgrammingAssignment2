## This R script contains two functions which will be used to 
## cache the inverse of a matrix.  This is part of the R Programming
## Coursera coursework.

## This function is used to create a special matrix object that can 
## cache its inverse.  

makeCacheMatrix <- function(x = matrix()) {
    ## i is the inverse of the matrix and is initialized to null
    i <- NULL
    ## setMatrix is a function that takes an arugment, y (matrix)
    ## and passes it into the x variable
    setMatrix <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## getMatrix will simply return the matrix, x, set with setMatrix
    getMatrix <- function () x
    ## setInverse will take as input an inverse matrix
    ## and pass it into the i variable
    setInverse <- function (inverseMatrix) i <<- inverseMatrix
    ## getInverse will simply return i, the inverse matrix
    getInverse <- function() i
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function actually computes the inverse of the matrix
## if it is not in cache already and it has not been modified
## since being cached.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    ## Check if the inverse was actually in cache. 
    ## If so, end early and return the value
    if(!is.null(i)) {
        message("Getting the inverted matrix from cache!")
        return(i)
    }
    matrixIn <- x$getMatrix()
    i <- solve(matrixIn, ...)
    x$setInverse(i)
    i
    }
