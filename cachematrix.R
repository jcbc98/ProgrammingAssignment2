## Efficient method for repeated matrix inverse calculations
## =========================================================
## The following two functions can be used to make repeated
## calculations of the inverse of the same matrix efficient
## by storing a "cached" value of the inverse and returning
## the cached value instead of recalculating the inverse of
## the same matrix. 
##
## By using a special "Cache Matrix" object, the original data
## and its inverse are nicely bundled together, eliminating 
## the need to keep track of and/or recalculate the inverse
## for a specific matrix throughout the code. Code using these 
## "Cache Matrix" objects do not even need to keep track of
## whether the inverse was already calculated before retrieving
## the cached inverse from the object.  Simply calling the
## cacheSolve function will correctly use the cached inverse
## when it is available and calculate and store the inverse
## when it has not been calculated for the current data matrix.
##
## -------------
## Example usage
## -------------
## > m <- makeCacheMatrix()   # create a special "Cache Matrix" object  
## > m$getinv()               # retrieve the inverse
## NULL                       # NULL is returned since inverse not calculated
## > m$set(matrix(rnorm(16), 4, 4))  # store a square matrix
## > m <- makeCacheMatrix(matrix(rnorm(16), 4, 4))  # equivalent to above
## > cacheSolve(m)                   # calculate the inverse
## ... (output based on m) ...
## > round(m$get() %*% cacheSolve(m), 3) # matrix multiply and round results
## getting cached data                   # message indicates cached inverse
##      [,1] [,2] [,3] [,4]
## [1,]    1    0    0    0       # the result of the matrix multiplication
## [2,]    0    1    0    0       # of the retrieved original matrix and its
## [3,]    0    0    1    0       # inverse is correctly the identity matrix
## [4,]    0    0    0    1

## ----------------------------------------------------------------------------
## makeCacheMatrix takes a matrix (optional; defaults to an empty matrix) and
## returns a list of functions which can be used to set and get the matrix
## object and set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    # initialize the cached inverse to NULL
    inv <- NULL
    # define the set function to assign the given matrix y as the value for
    # the x variable in the environment in which the set function is defined
    set <- function(y) {
        x <<- y           # overwrite the previous x value in parent environment
        inv <<- NULL      # reset the cached inverse to NULL in parent env.
    }
    # define the get function to return the x value in the parent environment 
    get <- function() x
    # define the setinv function to assign the given inverse as the value for
    # the inv variable in the parent environment
    setinv <- function(inverse) inv <<- inverse
    # define the getinv function to return the inv value in parent environment
    getinv <- function() inv
    # return the list of the defined functions
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}

## ----------------------------------------------------------------------------
## cacheSolve takes an object x created by the makeCacheMatrix function and
## returns the inverse of the matrix in x, by using a cached value if it has
## already been calculated for the matrix in x or by using solve() (passing
## along any additional arguments supplied to cacheSolve)
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # retrieve the inverse from the x object
    inv <- x$getinv()
    # test to see if the inverse is already calculated (i.e. not NULL)
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)                     # return cached inverse
    }
    # inverse was not already calculated, so...
    # retrieve the matrix data
    data <- x$get()
    # calculate the inverse (solve), passing along any extra parameters
    inv <- solve(data, ...)
    # store the inverse in the x object
    x$setinv(inv)
    # return the inverse
    inv
}
