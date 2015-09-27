## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {     ##taking the value and setting it to matrix x
                x <<- y
                inv <<- NULL
        }
        get <- function() x                                ##printing the matrix x
        setInverse <- function(inverse) inv <<- inverse    ##setting inverse
        getInverse <- function() inv                       ##printing inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()           
        if (!is.null(inv)) {                         ##if the inverse already exists
                message("retrieving cached data")    ##the matrix is retrieved from cache
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)    ##finding inverse of the given matrix
        x$setInverse(inv)         ##the inverse is set by calling the setInverse function
        inv                       ## inv (inverted matrix) is returned.
}
