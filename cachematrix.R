## There's two functions here that work together to 
## solve the inverse of a square matrix and cache 
## that result for later use so if I wanted to use that again 
## I wouldn't have to re-compute the inverse.

## The first -- makeCacheMatrix -- creates a list of four functions
## which set the matrix we're going to use, get the matrix (for solving later),
##  set the result of a solved matrix, and get the result of a solved matrix (the inverse).

makeCacheMatrix <- function(x) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This second function -- cachesolve -- takes the first function as an 
## argument, and --using its functions -- looks to see if it already has 
## the inverse of the matrix stored, if so it gives that cached result, 
## if not it gets the matrix and runs solve() on the matrix, 
## stores the result using the setinv() function
## from makeCacheMatrix, and returns the result.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
