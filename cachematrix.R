## Put comments here that give an overall description of what your
## functions do

## creates a vector, which is a list containing a function to
##set the value of the vector
##get the value of the vector
##set the value of the matrix inverse
##get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setSolve<- function(solve) m <<- solve
        getSolve<- function() m
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
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...) ## Return a matrix that is the inverse of 'x'
        x$setSolve(m)
        m
}
