## makeCacheMatrix initializes an invertible matrix object.
## cacheSolve finds the inverse of a makeCacheMatrix object
## and stores the inverse within that object.
## Note: If makeCacheMatrix contains a matrix and its inverse,
## and the matrix is changed, cacheSolve will return the old
## inverse unless setinv(NULL) or object$set(matrix) is run 
## beforehand to reset the value.

## Initialize a makeCacheMatrix object: 
## this function stores a matrix (which should be invertible).
## Contains functions to "set" and "get" the original matrix
## as well as functions "setinv" and "getinv" to set and store
## the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL  }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
  
    list(set = set, 
        get = get,
        setinv = setinv,
        getinv = getinv)  
}


## cacheSolve takes a makeCacheMatrix object as its argument,
## and returns the most recently stored inverse matrix, "inv".
## If there is no inverse matrix (NULL), the function solves
## for the inverse, saves it in the makeCacheMatrix object,
## and returns the inverse.

cacheSolve <- function(x) {
    inv <- x$getinv()
  
    if(!is.null(inv)) {
        message("getting cached inverse matrix")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinv(inv)
    inv
}
