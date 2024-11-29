# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse as NULL
  
  # Setter function for the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the cached inverse when matrix changes
  }
  
  # Getter function for the matrix
  get <- function() x
  
  # Setter function for the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Getter function for the inverse
  getInverse <- function() inv
  
  # Return a list of all functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the matrix or retrieve it from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Get the cached inverse
  
  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # Otherwise, calculate the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)       # Cache the computed inverse
  inv                     # Return the inverse
}

# Example usage:

# Create a special matrix object
specialMatrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))

# Calculate and cache the inverse
cacheSolve(specialMatrix)

# Retrieve the cached inverse
cacheSolve(specialMatrix)
