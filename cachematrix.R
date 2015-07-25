# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible
# Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        # check the cache for inverse of matrix
		inverse <- x$getinverse()
		if (!is.null(inverse))
		{
		     message("getting cached data for inverse matrix")
             return(inverse)
		}
		## if the inverse is not availabel in cache, compute the inverse of matrix
		data <- x$get()
		inverse <- solve(data)
		x$setinverse(inverse)
		inverse
}

## testing
## > source("cachematrix.R")
## > x=rbind(c(1,5),c(2,6))
## > m<-makeCacheMatrix(x)
## > m$get()
##     [,1] [,2]
## [1,]    1    5
## [2,]    2    6
## > cacheSolve(m)
##     [,1]  [,2]
## [1,] -1.5  1.25
## [2,]  0.5 -0.25
## > cacheSolve(m)
## getting cached data for inverse matrix
##     [,1]  [,2]
## [1,] -1.5  1.25
## [2,]  0.5 -0.25