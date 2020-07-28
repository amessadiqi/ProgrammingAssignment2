## This function stores a matrix in x and its inverse in m
## You can access the matrix calling get() and set its value calling set()
## You can access the inverse calling getinverse() and set its value calling setinverse()

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of a matrix if it is not yet calculated, else it just returns the cached value

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
        
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
