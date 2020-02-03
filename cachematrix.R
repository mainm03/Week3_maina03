## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix() is required to populate and or retrieve the inverse.
## inv is set to NULL, initialinzing it as an object that will be used later.
## X << y defines the deafult value of x as an empty numeric vector.
## The getters and setters set the data values within an object.
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

## This function computes the inverse of the above special "matrix", makeCacheMatrix. 
## It checks to see if the inverse has already been calculated in the environment, 
## then retrieves the inverse from the cache. If the inverse is not NULL, 
## it gets the matrix and calculates the inverse with solve.

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
