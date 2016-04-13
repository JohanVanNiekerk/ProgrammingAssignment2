# makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
# This is similar to the makeVector example, instead of mean we're using inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    #call to get will return the matrix x
    get <- function() x
    
    #set function to receive the inverse
    setinverse <- function(inverse) inv <<- inverse
    
    #get function to return inverse
    getinverse <- function() inv
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache..
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    
    #check if it exist
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    
    #if it existed we would have returned above so this is the else clause
    #perform the get of function x. This returns the matrix
    data <- x$get()
    
    #use solve to function to compute the inverse of matrix in 'data'
    inv <- solve(data)
    
    #calls the setinverse of function x and pass the inverse to it
    x$setinverse(inv)
    
    #returns the inverse
    inv
}