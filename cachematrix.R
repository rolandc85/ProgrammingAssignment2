## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    
    get <- function() x
    
    set_inv_mat <- function(y) inv_mat <<- y
    
    get_inv_mat <- function() inv_mat
    
    list(set = set, get = get, set_inv_mat = set_inv_mat, get_inv_mat = get_inv_mat)
}


## This function takes in a matrix and checks that if an inverse result of 

cacheSolve <- function(x, ...) {
        inv_mat <- x$get_inv_mat()
        
        if (!is.null(inv_mat)) {
            message("getting cached data")
            return(inv_mat)
        }
        
        data <- x$get()
        inv_mat <- solve(data)
        x$set_inv_mat(inv_mat)
        inv_mat
}
