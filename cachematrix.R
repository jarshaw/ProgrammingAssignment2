
## makeCacheMatris
## This function will calculate the inverse of a matrix and store to memory.

## cacheSolve
## This function will check to see if the inverse of a matrix has been found
## If it has, then it will not recalculate it, but will pull it from memory
## If any of the data in the matrix has changed the inverse will be re-done
## If the inverse has not been found (not in memory) then it will be 

##Function creates a matrix object that can calculate and cache its inverse
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

## Return a matrix that is the inverse of the X matrix
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