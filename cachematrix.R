## Eldher Hernandez 2016 -- R Programming -- Assigment 2


## This function creates an "Matrix Object" which members are the matrix and a list of functions to cache its inverse.
## Example: x <- makeCacheMatrix(matrix(rexp(9), 3))

makeCacheMatrix <- function(x = matrix()) {
    matrixInv <- NULL
    set <- function(y) {
        x <<- y
        matrixInv <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) matrixInv <<- Inverse # stores the inverse in a cached member with <<
    getInverse <- function() matrixInv #just prints the matrix inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This functions takes the matrix in object X and if its inverse is not cached, it calculates it and saves on cache, so next time
## no need to calculate again.
## Example: cacheSolve(x)

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached matrix inverse")
        return(m)
    }
    data <- x$get()
    matrixInv <- solve(data, ...)
    x$setInverse(matrixInv)
    matrixInv
        
}
