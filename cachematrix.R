## These functions together compute the inverse of a matrix, or retrieve the
## inverse of the matrix if it has already been computed been cached

## makeCacheMatrix's input is a matrix matx
## given a matrix, it creates a "vector," which is a list of functions
## a$get() returns the matrix
## a$set(new_matx) resets the matrix to new_matx
## a$setInverse() calls the function "solve" to calculate the inverse of matx
## a$getInverse() returns the inverse of matx if it has been cached 
## (see cacheSolve below)

makeCacheMatrix <- function(matx = matrix(,nrow = 0,ncol = 0)) {
        matxInv <- NULL
        set <- function(new_matx) {
                matx <<- new_matx
                matxInv <<- NULL
        }
        get <- function() matx
        setInverse <- function(matx) matxInv <<- matx
        getInverse <- function() matxInv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## cacheSolve takes the input a from a <-makeCacheMatrix(matx)
## if a$getInverse() is NULL, it computes the inverse of a$get and caches it
## if a$getInverse() has already been cached, it returns the stored inverse

cacheSolve <- function(x, ...) {
        matxInv <- x$getInverse()
        if(!is.null(matxInv)) {
                message("getting cached data")
                return(matxInv)
        }
        data <- x$get()
        matxInv <- solve(data, ...)
        x$setInverse(matxInv)
        matxInv
}
