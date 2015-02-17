## Caching the Inverse of a Matrix

## This function creates a special "matrix" object
## that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        ## operator `<<-` is used to assign a value to an object 
        ## in an environment that is different from the current environment
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above
cacheSolve <- function(x, ...) {

    m <- x$getInverse()
    
    ## If the inverse has already been calculated 
    ## (and the matrix has not changed), then `cacheSolve` should retrieve 
    ## the inverse from the cache
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    
    ## Computing the inverse of a square matrix with the `solve` function
    m <- solve(data, ...)
    x$setInverse(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
