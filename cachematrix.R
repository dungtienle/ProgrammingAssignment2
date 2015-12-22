## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## computes the inverse of the “matrix” returned by makeCacheMatrix()
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        ## If the inverse has already been calculated and the matrix has not changed, 
        ## it’ll retrieves the inverse from the cache directly.
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
