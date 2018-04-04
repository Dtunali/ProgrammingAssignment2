## makeCacheMatrix() creates an R object that stores a matrix and its inverse
## This R object is a list
## cacheSolve() retrieves the inverse from the cached value
## the retrieved object is a matrix, inverse of 'x'

## makeCacheMatrix() creates an R object that stores a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

        setInverse <- function(Inverse) m <<- Inverse

        getInverse <- function() m


        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve() retrieves the inverse from the cached value 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	  m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m

}
