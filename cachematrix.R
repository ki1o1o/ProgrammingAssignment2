# This assignments practices the use of the <<- operator which can be used to assign a value to 
# an object in an environment that is different from the current environment. Below are two functions 
# that are used to create a matrix and caches its inverse. Calculating the inverse of the
# same matrix in the future will retrieve the cached inverse instead of re-computing it.


# The function makeCacheMatrix creates a special "matrix", which is really a list 
# containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y 
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The following function cacheSolve calculates the inverse of the "matrix" created in makeCacheMatrix. 
# However, it first checks whether the inverse of the matrix has already been calculated and 
# cached. If so, it retrieves the inverse from the cache and skips the computation. Otherwise, 
# it calculates the matrix inverse and sets the value of the inverse in the cache via the 
# setinverse function.


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