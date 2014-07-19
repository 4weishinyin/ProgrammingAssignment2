## Matrix inversion is usually a costly computation; 
## it is benefit to caching the inverse of a matrix 
## instead of computing it repeatedly. 

## The "makeCacheMatrix" function below instantiates a "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        m <- matrix()
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The "cacheSolve" function below computes the inverse of the 
## "matrix" instantiated by the "makeCacheMatrix" above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(length(m) > 1) {
                message("Getting the cached matrix inversion data...")
                return(m)
        }
        data <- x$get()
        message("Computing the inversion of a matrix...")
        m <- solve(data, ...)
        x$setinv(m)
        m
}
