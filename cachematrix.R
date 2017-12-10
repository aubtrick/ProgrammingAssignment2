## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function, 'makeCacheMatrix', creates a special 'matrix' which is really
## a list containing a function to:
##
## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse of the matrix
## 4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ## Set the value of m to NULL; this will be used as a check for existing data
        set <- function(y) {
                x <<- y  ## Set x = y in the Global environment
                m <<- NULL  ## Reset M to NULL in the Global environment
        }
        get <- function() x  ## Get the value of the original matrix, 'x'
        setinv <- function(inv) m <<- solve(x)  ## Use the solve function to set the
                                                ## inverse of 'x'
        getinv <- function() m  ## Get the inverse of 'x'
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)  ## Assign each function as a named element in a list
}


## Write a short comment describing this function

## The following function calculates the inverse of the special 'matrix' created with the
## 'makeCacheMatrix' function.  It first checks if the mean has already been calculated.
## If yes, it 'gets' the inverse from the cache and skips the calculation. Otherwise,
## it calculates the inverse of the data and sets the value of the inverse in the cache
## via the 'setinv' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()  ## Set 'm' to the inverse stored in the cache, if it exists
        if(!is.null(m)) {  ## If 'm' is not NULL, use the value in 'm' and return
                message("getting cached data")
                return(m)
        }
        data <- x$get()  ##  Otherwise, set 'data' to the input matrix
        m <- solve(data, ...)  #Calculate the inverse of the input matrix and store it as 'm'
        x$setinv(m)  ## Use the 'setinv' function to set the inverse of the new matrix
        m  ##  Return 'm'
}
