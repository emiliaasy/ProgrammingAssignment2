## This is a pair of functions, "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix

## makeCacheMatrix is a function which creates a new matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        r <- NULL #inverse initialized
        set <- function(y) 
        {
          x <<- y
          r <<- NULL
        }
        get <- function() 
          x #return the matrix
        setInverse <- function(inverse) 
          r <<- inverse
        getInverse <- function() 
          r
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve is a function which computes the inverse of that "matrix" returned by makeCacheMatrix. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        r <- x$getInverse()
        if(!is.null(r)) 
        {
          message("getting cached data")
          return(r)
        }
        data <- x$get()
        r <- solve(data, ...) #calculate the inverse
        x$setInverse(r)
        r #return the matrix
}

