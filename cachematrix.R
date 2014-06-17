## makeCacheMatrix defines a matrix as a list of functions
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## set can be used to overwrite the 'x' data in the 'makeCacheMatrix'-environment that is called upon by 'cacheSolve'
        set <- function(y) { 
                x <<- y
                inv <<- NULL
                }
        ## get is used to retrieve the original data in the 'makeCacheMatrix'-environment for computing a new inverse
        get <- function() x
        ## setinv is used to store the new inverse from the 'cacheSolve'-environment in the 'makeCacheMatrix'-environment
        setinv <- function(inverse) inv <<- inverse
        ## getinv is used to load the stored inverse from the 'makeCacheMatrix'-environment into the 'cacheSolve'-environment
        getinv <- function() inv
        list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## cacheSolve first checks whether an inverse is already stored in the 'makeCacheMatrix'-environment
cacheSolve <- function(x, ...) {  
        inv <- x$getinv()
        ## If so, it returns this inverse with a message
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                }
        ## If not, it retrieves the matrix from the 'makeCacheMatrix'-environment, solves it, saves it, and returns it
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
