## These functions will calculate the inverse of a matrix. 
## If the inverse has already been solved, the function will pull from the cache. 

## This function creates a storage vector to allow the cacheInverse function to pull from the cache. 

makeCacheMatrix <- function(x = matrix()) {
    invMatrix <- NULL #inverted matrix. Set to null each time
    
    set <- function (y) {
      x <<- y
      invMatrix <<- NULL
    }

    #Storage functions
    get <- function() x
    setInv <-function(solve) invMatrix <<- solve  #Saves the original matrix to the storage vector using superassignment
    getInv <- function() invMatrix   #returns the cached value 
    
    list (set = set, get = get, 
        setInv = setInv, 
        getInv = getInv)
}


## This function computes the inverse of the matrix entered.  
## If the inverse has already been solved, it will return the cached value.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInv()
    if(!is.null(invMatrix)){
      message("getting cached data")
    return(invMatrix)
    }
    data <- x$get()
    invMatrix <- solve(data,...)
    x$setInv(invMatrix)
    invMatrix    
}
