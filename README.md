# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    set <- function(y) {
        x <<- y  # Assign the new matrix to x in the parent environment
        inv <<- NULL  # Reset the inverse cache
    }
    
    get <- function() x  # Function to return the original matrix
    setInverse <- function(inverse) inv <<- inverse  # Function to cache the inverse
    getInverse <- function() inv  # Function to get the cached inverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# cacheSolve: Computes the inverse of the matrix and caches it if not already cached.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Retrieve the cached inverse
    
    if (!is.null(inv)) {  # If inverse is already cached, return it
        message("Getting cached inverse")
        return(inv)
    }
    
    data <- x$get()  # Get the original matrix
    inv <- solve(data, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the computed inverse
    inv  # Return the inverse
}
