## This is a pair of functions that cache the inverse of a matrix
## so computation can be sped up if it has already been solved once


##makeCacheMatrix creates a 'fake' matrix, 
##which is really a list containing another set of functions
makeCacheMatrix <- function(x = matrix()) {
        
        #If you're running this it's assumed x is a new matrix, so we will
        #set a stored value for the inverse as NULL, flagging
        #that the inverse has not been calculated yet
        inv <- NULL
        
        #create a function to store a value the parent environment
        #also make the inverse flag null in the parent environment
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        # function to retrieve the original matrix from current environment
        get <- function() x
        
        # function to solve matrix and store in the parent environment
        setinv <- function(solve) inv <<- solve
        
        # function to retrieve the inversed in current environment
        getinv <- function() inv
        
        # create our 'fake' list
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##cacheSolve calculates the inverse of the fake matrix created above.
##It first checks to see if the inverse has already been calculated  
##using the NULL value as a flag

cacheSolve <- function(x, ...) {
        
        
        # retrieve the inversed value from the parent environment
        inv <- x$getinv()
        
        ##Checks if inverse has already been calculated.
        ##If it's not null, give a message that we are using
        ##cached data and simply return the already stored inverse
        if(!is.null(inv)) {
                message("Getting Cached Data")
                return(inv)
        }
        
        #If it is null, get the matrix and proceed with calculation
        data <- x$get()
        inv <- solve(data, ...)
        
        #stores inverted matrix as 'x' in parent environment
        x$setinv(inv)
        
        ## Returns the matrix that is the inverse of 'x'
        inv

}
