## The two functions in this script are used to calculate and cache
## the inverse of a matrix.

## makeCacheMatrix produces a list containig functions that:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list( set = set, get = get, setinverse = setinverse, 
          getinverse = getinverse)
}

## The function cacheSolve return the inverse of the matrix.
## First a check is performed to see if the inverse has already been computed,
## and if true it retrieves the cached value.
## If the inverse hasn't been computed earlier, it is computed and cached
## by using the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
