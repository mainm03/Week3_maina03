## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
    }



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getInverse()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
    }

## Testing my matrix.

 aMatrix <- makeCacheMatrix(matrix(c(-2, 4, 1, -3),2,2))
 
# aMatrix$set(matrix(c(-2, 4, 1, -3),2,2)) 
 aMatrix$get()
 aMatrix$getInverse()
 
## Testing the inverse of the matrix.
cacheSolve(aMatrix)
cacheSolve(aMatrix)
aMatrix$getInverse()
aMatrix$set(matrix(c(4, 4, 2, 8),2,2)) 
aMatrix$get()
cacheSolve(aMatrix)