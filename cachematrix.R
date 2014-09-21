## Below are two functions designed to cache the inverse of a matrix as per the R Programming Coursera course 
## second programming assignment on lexical scoping. Steps to test the efficacy of the assignment are shown at
## the end of the function definitions (bottom of page)
##

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,               ## Create a list of the items
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## This function returns the inverse of the above matrix. If the inverse has previously been computed
## the function returns the cached inverse. If it has not previously been computed, the inverse is computed
## and stored in the cache for subsequent executions.

cacheSolve <- function (x = matrix(), ...) {
    m <- x$getmatrix()                        
    if(!is.null(m)) {                         ## As long as m is not NULL
        message("Getting the cached data...") ## Print out that the cached matrix is being used
        return(m)                             ## Print the matrix
    }
    matrix <- x$get()                         ##
    m <- solve(matrix, ...)                   ##
    x$setmatrix(m)                            ##
    m                                         ## Print the matrix
}

## !!!How to test
## > x = rbind(c(66, 88), c(88, 66))
## > x
##      [,1] [,2]
## [1,]   66   88
## [2,]   88   66
## > 
## > m = makeCacheMatrix(x)
## > m$get()
##      [,1] [,2]
## [1,]   66   88
## [2,]   88   66
## > 

## !!!NOTE: the caching doesn't happen until the second execution
## > cacheSolve(m)
##             [,1]        [,2]
## [1,] -0.01948052  0.02597403
## [2,]  0.02597403 -0.01948052

## !!!Retrieving from the cache in the second execution
## > cacheSolve(m)
## Getting the cached data...
##             [,1]        [,2]
## [1,] -0.01948052  0.02597403
## [2,]  0.02597403 -0.01948052

## !!!Retrieving from the cache in the third exectuion
## > cacheSolve(m)
## Getting the cached data...
##             [,1]        [,2]
## [1,] -0.01948052  0.02597403
## [2,]  0.02597403 -0.01948052