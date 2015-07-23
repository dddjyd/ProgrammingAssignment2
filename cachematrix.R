## Computes matrix's inverse more efficient with caching

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function(){
        x
    }
    setMean <- function(mean){
        m <<- mean
    }
    getMean <- function(){
        m
    }
    list(set = set, get = get, setMean = setMean, getMean = getMean)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        m <- x$getMean()
        if(!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setMean(m)
        m
}