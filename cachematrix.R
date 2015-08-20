## This program is to cache the inverse of a square matrix 

## The first function is to create a "matrix" object 
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) s <<- solve
        getinverse <- function() s
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}


## The second function is to compute the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## check if the inverse has been calculated. If s is not NULL,
        ## return the cached inverse matrix
        s <- x$getinverse()
        if(!is.null(s)) {
                message('getting cashed data')
                return(s)
        }
        ## calculate the inverse for matrix 'x'
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}
