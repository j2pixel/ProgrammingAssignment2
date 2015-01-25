makeCacheMatrix <- function(x = matrix()) {
        
        inv_x <- NULL 
        
        set <- function(y) {
                x <<- y
                inv_x <<- NULL # it also initialises inv_x to null
        }
        get <- function() {
                x  # return the input matrix
        }
        setInv <- function(inv) {
                inv_x <<- inv  # set the inversed matrix        
        } 
        getInv <- function() {
                inv_x # return the inversed matrix
        }
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


cacheSolve <- function(x, ...) {
        m <- x$getInv() # Get x's cached inversed matrix
        # This will return NULL if it has not been calculated
        
        if(!is.null(m)) { # If The inverse is cached (Not NULL)
                message("getting cached data")
                return(m) # return the cached inverse
        }
        
        data <- x$get() # if not, we do x$get to get the matrix object
        m <- solve(data) # we solve it
        x$setInv(m) # we then set it to the object
        m # return the solved result
}