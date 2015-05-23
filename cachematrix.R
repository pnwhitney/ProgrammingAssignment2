## These functions provide the ability to calculate and cache the result of 
## the inversion of a matrix. Caching capabilities are provided to reduce 
## the overhead of recalculating the inversion of a matrix if the matrix
## has not changed since the prior invocation of cacheSolve.

## MakeCacheMatrix creates functions which allow access, setting, and change
## detection of the cached matrix
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
## If the cached matrix exists the function tests to see of the cached
## matrix has been changed. If NOT changed then the cached matrix inversion
## is returned. If the inversion has not been solved or if the matrix has
## been changed then the matrix inversion is calculated and stored in cache.
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
