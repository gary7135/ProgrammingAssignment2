## Put comments here that give an overall description of what your
## functions do

## creates a vector, which is a list containing a function to
##set the value of the vector
##get the value of the vector
##set the value of the matrix inverse
##get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setSolve<- function(solve) s <<- solve
        getSolve<- function() s
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## Calculate the inverse of the matrix that's created in makeCacheMatrix function.
## It first checks to see if the matrix inverse has already been calculated. 
## If so, it gets the inverese from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value 
## of the matrix in the cache via the setSolve function.


cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        matrix <- x$get()
        s <- solve(matrix, ...) ## Return a matrix that is the inverse of 'x'
        x$setSolve(s)
        s
}
