
## makeCacheMatrix function stores a matrix and its inverse.
## It builds a list which contains set(), get(), setinverse(), and getinverse() functions.
## set() - assign passed argument to x object and set value of i object to NULL
## (to clear previous value of m that had been cached)
## get() - retrieves the value of the matrix x from the parent environment of the makeCacheMatrix()
## setmean() - assign the input argument to the value of i in the parent environment
## getmean() - retrieves the value of i from the parent environment
## makeCacheMatrix() assigns each function to a list and passed it to the parent environment


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve() retrieves the inverse of the input matrix from an object of type makeCacheMatrix()
## If value of inverse is not equal to NULL in the makeCacheMatrix(), use getinverse to retrieve the value of inverse
## If value of inverse is equal to NULL in the makeCacheMatrix(), compute inverse then use setinverse to set the inverse 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
