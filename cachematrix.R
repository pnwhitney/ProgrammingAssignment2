## These functions provide the ability to calculate and cache the result of 
## the inversion of a matrix. Caching capabilities are provided to reduce 
## the overhead of recalculating the inversion if the matrix has not changed 
## since the prior invocation of cachedSolve.

## MakeCacheMatrix creates functions which allow access and setting of the 
## cached matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        changed <- FALSE
        set <- function(y) {
                x <<- y
                m <<- NULL
                changed <<- TRUE
        }
        get <- function() x
        setinv <- function(inv) { 
            changed <<- FALSE
            m <<- inv
        }
        getinv <- function() m
        getChanged <- function() changed <<- changed
        list(set = set, 
             get = get,
             setinv = setinv,
             getinv = getinv, 
             getChanged = getChanged)
}


## CacheSolve accesses the cache to see if the cached matrix exists.
## If the cached matrix exists it is compared agains the parameter
## matrix to assure that the matrix content have not changed. If the
## matrix contents have not changed the the cached inverse is returned
## otherwise the imatrix inverse is calculated, stored in cache and
## retured. 
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m) && !x$getChanged()) {
        message("getting cached matrix inverse")
        return(m)
    }
    data <- x$get()
    message("calculating matrix inverse")
    m <- solve(data)
    x$setinv(m)
    m
}
