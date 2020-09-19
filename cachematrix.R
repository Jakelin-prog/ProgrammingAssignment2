## Catching the inverse of a matrix 

## This function creates a special "matrix" object that can cache its inverse.
## 1. Set the value of the matrix "x"
## 2. Get the values of the matrix "x" 
## 3. Set the inverse matrix "m"
## 4. Get the inverse matrix "m"

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverso <- function(inverso) m <<- inverso
    getinverso <- function() m
    list(set = set, get = get,
        setinverso = setinverso,
        getinverso = getinverso)
}


##  This function computes the inverse of the special "matrix" returned 
##  by makeCacheMatrix above. If the inverse has already been calculated 
##  (and the matrix has not changed), then cacheSolve should retrieve the 
##  inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverso()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverso(m)
    m
}
