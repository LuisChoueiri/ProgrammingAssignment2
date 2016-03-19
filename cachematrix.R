
## My functions will cache the inverse of the given matrix, removing calculations
## and replacing them with calls to a saved value

## This function will create a cached version of an inputed matrix


makeCacheMatrix <- function(x = matrix()) {
        inv <<- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setsolve <- function(solve) inv <<- solve
        getsolve <- function() inv
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function will use the solve funtion to calculate the inverse of the cached matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(x, ...)
        x$getsolve(inv)
        inv
}
