## These functions create special matrix, and compute inverse of this matrix.
## If the inverse matrix was already computed, function returns cached data.

## makeCahceMatrix creates special type of "matrix" which is the list, consisting of 4 elements;
#each element is function to set initial values to matrix and its inverse, get initial matrix, 
#set inverse of the matrix and get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() x
    set_inv <- function(inv) inv_matrix <<- inv
    get_inv <- function() inv_matrix
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## cacheSolve returns a matrix that is the inverse of 'x'
## At the beginning function checks if the inverse of the matrix was already calculated and if yes - returns 
## calculated inverse of matrix from cache. Othrwise function calculates inverse of the given matrix using 
## R function solve(X)

cacheSolve <- function(x, ...) {
    
    inv_matrix <- x$get_inv()
    if(!is.null(inv_matrix)) {
        message("getting cached data")
        return(inv_matrix)
    }
    data <- x$get()
    inv_matrix <- solve(data, ...)
    x$set_inv(inv_matrix)
    inv_matrix
}
