## R function to create Matrix inversion to cache the inverse of a matrix and to avoid recomputing.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mtx = matrix()) {

# Create inverse and initialize to null

    invr <- NULL

# set the matrix . 

    setmtx <- function(mx) {
            mtx <<- mx
            invr <<- NULL
    }

    # Gets the matrix 

    getmtx <- function() mtx

    # set inverse

    setinverse <- function(inverse) invr <<- inverse

    # Get the inverse

    getinverse <- function() invr

    # create list of above functions

    list(setmtx = setmtx, getmtx = getmtx,
         setinverse = setinverse,
         getinverse = getinverse)	
}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mtx, ...) {

    invr <- mtx$getinverse()

    # if computed return the inverse

    if(!is.null(invr)) {
    		
        message("Return cached matrix")

        return(invr)
    }

    # If not computed get matrix and find inverse and return it

    data <- mtx$getmtx()

    invr <- solve(data, ...)

    mtx$setinverse(invr)
     
     message("Return the inverse")

    invr    
}