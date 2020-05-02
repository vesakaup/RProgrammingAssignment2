
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        set_inv <- function(x_inv) inv <<- x_inv
        get_inv <- function() inv
        list(set = set, get=get, set_inv = set_inv, get_inv = get_inv)
                
        

}


## This function computes the inverse of the matrix returned by 
## makeCacheMatrix 
## If the inverse has already been calculated 
## the function retrieves the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
                
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inv(inv)
        inv
        
}
