library(matlib)

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # initialises an object to hold global matrix variable and its inverse
    # default argument is an empty matrix. Either call init_cachedmatrix or call
    # the set and setInverse methods to assign the matrix and its inverse
    inverseMatrix <- NULL
    setMatrix <- function(newMatrix){
    # initialises or reassigns a matrix, argument is a square invertible matrix
        x <<- newMatrix
    }
    getMatrix <- function(){
    # returns the assigned matrix
        return(x)
    }
    setInverse <- function(){
    # calculates and stores the inverse matrix as an attribute of the object
        inverseMatrix <<- matlib::Inverse(x)
    }
    getInverse <- function(){
    # returns the inverse matrix to avoid recalculation
        return(inverseMatrix)
    }
    list(set = setMatrix, get = getMatrix,
         setInverse = setInverse, getInverse = getInverse)
    # renamed set to shorten method variable names
}

init_cachedmatrix <- function(x = matrix()){
    # function is to initialise the attributes of the holder object
    # function returns the holder object with initialised matrix stored in it
    xmat <- makeCacheMatrix()
    xmat$set(x)
    xmat$setInverse()
    return(xmat)
}

## Write a short comment describing this function

cacheSolve <- function(xmat) {
    # function gets the inverse of a stored matrix, or calculates, stores and
    # returns it if inverse does not exist
    # argument is the object holding the required matrix
    inverseMatrix <- xmat$getInverse()
    if (!is.null(inverseMatrix)){
        message("Getting cached data.")
        return(inverseMatrix)
    }
    xmat$setInverse()
    return(xmat$getInverse())
    ## Return a matrix that is the inverse of 'x'
}

#testing=======================================================================

# xmat <- makeCacheMatrix()
# xmat$set(matrix(1:4, 2, 2))
# print(xmat$get())
# # xmat$setInverse()
# print(xmat$getInverse())

# xmat <- init_cachedmatrix(matrix(1:4, 2, 2))
# print(xmat$get())
# print(xmat$getInverse())
xmat <- makeCacheMatrix()
xmat$set(matrix(1:4, 2, 2))
print(xmat$get())
print(cacheSolve(xmat))

#example function for calcing mean=============================================