## Caching the Inverse of a Matrix

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        m_inverse <- NULL
        set <- function (y) {
                x <<- y 
                m_inverse <<- NULL
        }
        get <- function () x
        setinverse <- function (inv) m_inverse <<- inv
        getinverse <- function () m_inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}


## cacheSolve function computes the inverse of matrix returned by makeCacheMatrix.
## If the inverse has already been calculated, it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inverse <- x$getinverse()
        if(!is.null(m_inverse)) {
                message("getting cached data")
                return(m_inverse)
        }
        data <- x$get()
        m_inverse <- solve(data,...)
        x$setinverse(m_inverse)
        m_inverse
}
