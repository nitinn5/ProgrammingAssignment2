
## function that looks for the computation in cache first before redoing it. 
makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## Checks if the function was already run on the matrix, and then computes its inverse if not available
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- inv(data, ...)
        x$setinverse(i)
        i
}

