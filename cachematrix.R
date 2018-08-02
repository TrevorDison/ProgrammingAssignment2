## First function (makeCacheMatrix) creates a list of four functions that 
##  can be called to set or get a matrix or its inverse.
## Second function (cacheSolve) calculates or retrieves the inverse depending
##  on whether it is already stored in memory.


## Create a list of the functions needed to set/get a matrix and its inverse
##  First function stores the matrix (X) in memory and sets N to NULL.
##  Second function retrieves the matrix (X) from memory.
##  Third function stores the inverse (N) in memory.
##  Fourth function retrieves the inverse (N) from memory.

makeCacheMatrix <- function(X = matrix()) {
    N <- NULL
    set <- function(Y) {
        X <<- Y
        N <<- NULL
    }
    get <- function() X
    setinverse <- function(inverse) N <<- inverse
    getinverse <- function() N
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculate or retrieve the inverse matrix
## If the inverse is not null (already exists in memory), then the function
##  retrieves the inverse. Otherwise, it calculates and returns the inverse.

cacheSolve <- function(X, ...) {
    N <- X$getinverse()
    if(!is.null(N)) {
        message("getting cached data")
        return(N)
    }
    data <- X$get()
    N <- solve(data, ...)
    X$setinverse(N)
    N
}

