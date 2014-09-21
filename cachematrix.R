## Below are two functions designed to cache the inverse of a matrix as per the R Programming Coursera course 
## second programming assignment on lexical scoping. Steps to test the efficacy of the assignment are shown at
## the end of the function definitions (bottom of page)
##

## This function creates a special "matrix" object that can cache its inverse. This is occurs by performing
## the following steps:
## set the value of the matrix
## get the value of the matrix
## set the matrix's inverse value 
## get the matrix's inverse value 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {                     ## set the value of the matrix
        x <<- y                              ## assign the value of y to x
        m <<- NULL                           ## assign a NULL value to m
    }
    get <- function() x                      ## get the value of the matrix
    setmatrix <- function(solve) m <<- solve ## set the value of the matrix
    getmatrix <- function() m                ## get the value of the matrix
    list(set = set, get = get,               ## Create a list of the items
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## This function returns the inverse of the above matrix. If the inverse has previously been computed
## the function returns the cached inverse. If it has not previously been computed, the inverse is computed
## and stored in the cache for subsequent executions.

cacheSolve <- function (x = matrix(), ...) {  ## The begining of the cacheSolve function
    m <- x$getmatrix()                        ## 
    if(!is.null(m)) {                         ## As long as m is not NULL
        message("Getting the cached data...") ## Print out that the cached matrix is being used
        return(m)                             ## Print the matrix
    }
    matrix <- x$get()                         ## get the inverse of the matrix
    m <- solve(matrix, ...)                   ##  
    x$setmatrix(m)                            ## set the inverse of the matrix
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
##
## <EOF>