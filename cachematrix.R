## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  ## inv is inverse of the matrix and it's reset to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x  ## returns the value of original matrix
        setinv <- function(inverse) inv <<- inverse  ## called by cacheSolve()
        getinv <- function() inv  ## return the cached value
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()  ## get the value of the inverse
        if(!is.null(inv)) {
                message("getting cached data")  ## send this message to the console
                return(inv)  ## return the inverse
        }
        data <- x$get()  ## reach this code only if x$getinv() is NULL
        inv <- solve(data, ...) ## calculate inverse
        x$setinv(inv)  ## store the calculated inverse in x
        inv  ## return the inverse
}
