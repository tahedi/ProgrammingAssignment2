# cache potentially time-consuming computations. For example, taking
## the inverse of a numeric matrix is typically a costly computation operation. 
## If the contents of a matrix are not changing, it may make sense to cache the 
## value of the inverse so that when we need it again, it can be looked up in the
## cache rather than recomputed. 


## The first function, makeVector creates a special "matrix", 
## * which is really a list containing a function to
## * set the value of the matrix
## * get the value of the matrix
## * set the value of the inveMatrix
## * get the value of the inveMatrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInvMatrix <- function(invMatrix) m <<- invMatrix
    getInvMatrix <- function() m
    list(set = set, get = get,
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)   
}

## The following function calculates the invMatrix of the special "matrix" 
## created with the above function. However, it first checks to see 
## if the invMatrix has already been calculated. If so, it gets the invMatrix
## from the cache and skips the computation. Otherwise, it calculates 
## the invMatrix of the data and sets the value of the invMatrix in the cache 
## via the setInvMatrix function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInvMatrix ()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInvMatrix(m)
    m
}
# 
cacheSolve(makeCacheMatrix(matrix(c(1,0,0,1),2,2)))