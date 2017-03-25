## Programming assignment 2 : Caching the inverse of matrix using the advantage of 
## lexical scoping of R.
## The below assignment consist of two functions namely makeCacheMatrix and cacheSolve.


## makeCacheMatrix will create a "special" vector which returns a list
## having the following function. 
## To get the value of  matrix
## To set the value of matrix
## To get the inverse of the matrix
## To set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
        
}


##  The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function and returns the output.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        message("calulating inverse for first time")
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        m
}


