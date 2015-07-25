## Use these functions to invert a matrix.
## The functions will cache the matrix, when the matrix was already inverted. 
## This safes computation time and makes the function faster. 

## The "makeCacheMatrix" will test if the input matrix already has been
## inverted or not. If yes, the "cacheSolve" function will cache the matrix
## and gives the already existing matrix as an output. If no, the "cacheSolve"
## function will inverse the matrix and gives that inverted matrix as output.

## The "makeCacheMatrix" works like this: the "x" is the input matrix. 
## The output of the function is a list of the following functions: "get", 
## "set", "setsolve", and "getsolve". "get" obtains the input matrix "x". 
## "set" can be used to set the another matrix "y", which has already been 
## inverted. This sets "s" NULL meaning that the "getsolve" function returns 
## NULL. 

makeCacheMatrix <- function(x = matrix()) {		
            s <- NULL
            set <- function(y) {		
                    x <<- y
                    s <<- NULL
            }
            get <- function() x
            setsolve <- function(solve) s <<- solve
            getsolve <- function() s
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
 }

## The "cacheSolve" works like this: when the "getsolve" function has a value
## of NULL, the matrix was already inverted and the function will just return 
## already inverted matrix. Else, matrix "x" will be stored in "data" and 
## inverted by the solve() function. The inverted matrix "s" returns. 

cacheSolve <- function(x, ...) {
            s <- x$getsolve()
            if(!is.null(s)) {
                    message("getting cached data")
                    return(s)
            }
            data <- x$get()
            s <- solve(data, ...)
            x$setsolve(s)
            s
 }

