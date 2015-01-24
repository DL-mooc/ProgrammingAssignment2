## These functions allow to cache the value of the inverse of a matrix,
## so it doesn't need to be computed again if it's needed several times
## and the programmer doesn't have to save the inverse manually
## in the environment of her program

## Makes a new matrix capable of caching it's inverse
## 'original_matrix' should be an invertible matrix
## To get the inverse of this new matrix, use 'cacheSolve' function
makeCacheMatrix <- function(original_matrix = matrix()) {
    inverse_matrix <- NULL
    set <- function(y) {
        original_matrix <<- y
        inverse_matrix <<- NULL
    }
    get <- function() original_matrix
    setinverse <- function(inverse) inverse_matrix <<- inverse
    getinverse <- function() inverse_matrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns a matrix that is the inverse of 'x'
## 'x' is a matrix capable of caching it's inverse (made with
## the 'makeCacheMatrix' function)
cacheSolve <- function(x, ...) {
    inverse_matrix <- x$getinverse()
    if(!is.null(inverse_matrix)) {
        #inverse already calculated
        return(inverse_matrix)
    }
    #inverse hadn't been calculated
    original_matrix <- x$get()
    inverse_matrix <- solve(original_matrix, ...)
    x$setinverse(inverse_matrix)
    inverse_matrix
}
