## Put comments here that give an overall description of what your
## functions do
#
#These two functions will calculate the inverse of a matrix. First, it will see if the inverse
#of the matrix already exists in cache otherwise, it will calculate it.
#
## Write a short comment describing this function
#makeCacheMatrix will create the getters and setters. It will return a list containing 4 functions.
#it should be initialised as follows:
#myMatrix <- makcCacheMatrix(matrix(c(1,2,3,4),2,2))

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) i <<- inverse
    
    getInverse <- function() i
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
#cacheSolve will take an argument of type makeCachMatrix and will return the inverse from cache,
#if it already exists or calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if (!is.null(m)){
        message("Getting cached inverse of matrix ...")
        return(m)
    } 
    data <- x$get()
    m <- solve(data)
    x$setInverse(m)
    m
}
