## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    ## Function to calculate the inverse of a Matrix
    ## Will return a list of functions to set/get the matrix and set/get the inverse matrix
    ## returned list will be used later with the cacheSolve() function
    
    inv <- NULL
    
    set <- function(y) {
        
        # operator "<<-" will asign a valued to the object in a enviroment diferent of the actual enviroment 
        x <<- y
        
        inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inverse) inv <<- inverse
    
    getinverse <- function() inv
    
    list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    ## this function will used as input the output of the function makeCacheMatrix()
    
    inv <- x$getinverse()
    
    #if the inverse matrix was already calculated will get it from the cache and does not calculates it again
    if(!is.null(inv)) {
        
        message("getting cached data")
        
        return(inv)
    }
    
    # if the inverse was not already calculated will proceed to calculate it
    data <- x$get()
    
    inv <- solve(data, ...)
    
    #uses setinverse fucntion to set the inverse matrix valu  in the cacche
    x$setinverse(inv)
    
    return(inv)
    
}
