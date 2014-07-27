## Put comments here that give an overall description of what your
## functions do
## This pair of cunctions cache the inverse of a matrix.
## Both functions assume that the matrix supplied is always invertible.
## Here is a sample usage of the functions.
## x<-diag(3)
## z <-makeCacheMatrix(x)
## zz<-cacheSolve(z)

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inversed <- NULL
        set <- function(y) {
                x <<- y
                inversed <<- NULL
        }
        get <- function() x
        setinversed <- function(invert) inversed <<- invert
        getinversed <- function() inversed
        list(set=set,get=get, setinversed=setinversed,getinversed=getinversed)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inversed <- x$getinversed()
        if (!is.null(inversed)) {
                message("getting cached matrix inverse")
                return(inversed)
        }
        inversed <- solve(x$get())
        x$setinversed(inversed)
        inversed
}
