## The function will calculate the inverse of a square matrix, store it to memory and can 
## check to see if it has already been calculated from which it will pull it from memory

## makeCacheMatris
## This function creates a matrix object that can calculate and cache its inverse
makeCacheMatrix <- function(X = matrix()) {
    inv <- NULL
    ## Inverse is set to NULL
    set <- function(y) {
        X <<- y
        inv <<- NULL
    }
    get <- function() X
    ## Inverse is calculated
    setinverse <- function(solve) inv <<- solve 
    getinverse <- function() inv
    ## Inverse is stored
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve
## This function will check to see if the inverse of a matrix object has been calculated
## If it has, then it will not recalculate it, but will pull it from memory (cache)
## If any of the data in the matrix has changed the inverse will be re-calculated and cached
## If the inverse has not been cacluated previously then it will be 
cacheSolve <- function(X, ...) {
    inv <- X$getinverse()
    ## If Inverse has already been calculated and is not different, 
    ## it will be pulled from memory
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    ## Calculates the inverse if it hasn't been done (NULL) or data changed
    mat <- X$get()
    inv <- solve(mat, ...)
    X$setinverse(inv)
    inv
}