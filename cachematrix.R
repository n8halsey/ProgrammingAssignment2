## Create two functions that cache the matrix's inverse

## Create a function that creates a matrix that is a special object to cache the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        set_inverse <- function(matrixInverse) inverse <<- matrixInverse
        get_inverse <- function() inverse
        list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Create a function that calculates the inverse of the matrix special object that is returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        inverse <- x$get_inverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$set_inverse(inverse)
        inverse
}