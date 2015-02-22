## Matrix inversion can be costly. These two functions cache the result of
## inverting a matrix. When the inverse function (`cacheSolve`) is called on
## an already inverted matrix, the cached version is returned.


## Creates a special "matrix" object that can cache the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
        set <- function(y) {
                x <<- y
                inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse_matrix <<- inverse
        getinverse <- function() inverse_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by `makeCacheMatrix`
## If the inverse has already been calculated (and the matrix has not changed),
## it retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_matrix <- x$getinverse()
        if(!is.null(inverse_matrix)) {
                message("getting cached data")
                return(inverse_matrix)
        }
        data <- x$get()
        inverse_matrix <- solve(data, ...)
        x$setinverse(inverse_matrix)
        inverse_matrix
}
