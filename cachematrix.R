## The below functions have been written to compute and cache the inverse of a matrix
## as part of R programming coursera course Week 3's assignment. makeCacheMatrix & cacheSolve functions 
## follow the logic and instructions given in the Readme file

## Creates a special object to hold the matrix and its inverse with functions to set and call both 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL             #Intially sets inverse as NULL
        set <- function(y) {    #function to set the original matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x             #Function to retrieve the original matrix
        setinv <- function(inverse) inv <<- inverse     #Function to cache the inverse matrix
        getinv <- function() inv        #Function to get the inverse matrix if already cached. Returns NULL if not cached
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function retrieves the cached inverse of matrix if available. 
## Else, it computes the inverse of the matrix and caches them once compute

cacheSolve <- function(x, ...) { 
        inv <- x$getinv()       #Retrieves cached inverse from makeCacheMatrix object
        if(!is.null(inv)) {     #Checks if retrieved cached matrix is NULL
                message("Getting cached data")
                return(inv)     #Returns the Cached inverse matrix
        }
        mat <- x$get()          #Retrieves the origial matrix
        message("Calculating Inverse")  
        inv <- solve(mat, ...)  #Calculates the inverse using solve() function
        x$setinv(inv)           #Caches the inverse of the function calculated to the special object
        inv     ## Return a matrix that is the inverse of 'x'
}
