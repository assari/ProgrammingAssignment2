## Create two functions which will allow speedier operations by caching
## the inverse of a matrix

## The makeCacheMatrix function returns a list containing sets and gets functions
## which will allow for a cached copy of the inversed of a given matrix
## to be set and retrieved
makeCacheMatrix <- function(x = matrix()) {

    # First create a NULL variable so that we don't get an error on first use
    invse <- NULL
    
    # We need a set function to allow us to set a new matrix to calc it's inverse
    # Whatever value of invse now is no longer valid, so set it to NULL
    set <- function(y)
    {   
        x <<- y
        invse <<- NULL
    }
    
    ## We also need a get function to complement the set function
    ## simply returns the current matrix in the environment
    get <- function()
    {   
        x
    }
    
    ## Now we need a setinverse function to manually set the inverse in the cache
    setinv <- function(inverse)
    {   
        invse <<- inverse
    }
    
    ## Lastly we need a getinv function to simply return the current inverse
    ## from the cache
    getinv <- function()
    {   
        invse
    }
    
    ## returns the functions in a list for further use
    list(set=set, get=get, setinv=setinv, getinv=getinv)
    
}


## This function allows for a much faster operations
## as it allows us to use cached inverse rather then
## having to calculate it each time. The cached inverse can
## either be calculated using solve or manually set

cacheSolve <- function(x, ...) {
    
    ## Gets the inverse of a matrix from the cache (if availabe, NULL otherwise)
    invse <- x$getinv()
    
    # if invse is not NULL, ie already exist in cache then no calc needed
    if (!is.null(invse)){
        message("getting cached data")  # Let us know value is from cache
        return(invse)
    }
    
    # If inverse not yet available, calculate inverse using the solve function 
    myMatrix = x$get()
    invse = solve(myMatrix, ...)
    
    # Don't forget to set calculated inverse into the cache (for next time).
    x$setinv(invse)
    
    return(invse)
}
