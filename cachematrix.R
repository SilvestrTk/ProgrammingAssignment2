## Functions for caching the inverse matrix for later use without need
## to count it again. In the case of large matrix it can save a time



## Setting the basic functions for caching the matrix and storing the inverted
## matrix

makeCacheMatrix <- function(x = matrix()) {
    ## default inverse matrix
    m_inv <- NULL
    ## setting new matrix and reseting inverse matrix
    set <- function(y) {
        x <<- y
        m_inv <<- NULL
    }
    
    get <- function() x
    ##setting inverse matrix
    setInv <- function(inverse) m_inv <<- inverse
    ##getting inverse matrix, if any
    getInv <- function() m_inv
    ##return list of the functions
    list(set = set,
         get = get,
         
         setInv = setInv,
         getInv = getInv)
}


## Solving matrix, firt check if the inverse is cached, if not computing
## Inversed matrix and storing it to cache for later use

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## If cached return cached data
    m_inv <- x$getInv()
    if (!is.null(m_inv)) {
        message("getting cached data")
        return(m_inv)
    }
    m <- x$get()
    ## if not cached, compute the inversion
    m_inv <- solve(m, ...)
    x$setInv(m_inv)
    m_inv
}
