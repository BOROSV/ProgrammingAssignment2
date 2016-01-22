# We wish to avoid costly computations, like Matrix inversions. 
# We therefore cache the inverse matrix so that we do not have 
# to recalculate it when not necessary.
# The function makeCacheMatrix:
# 1) sets the value of matrix 2) gets the value of matrix
# 3) sets value of the inverse of matrix, 4) gets the 
# value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
# The function cacheSolve returns the inverse of a matrix. 
# If inverse has been calculated already it collects the results.
# If it has not been calculated before it first computes inverse
# and then caches the value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inver <- x$getinverse()
    if(!is.null(inver)) {
        message("getting cached data")
        return(inver)
    }
    data <- x$get()
    inver <-solve(data)
    x$setinverse(inver)
    inver
}
