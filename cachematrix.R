# makeCacheMatrix creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Store the original matrix
  m <- x
  
  # Inverted matrix
  inv <- NULL
  
  # Function to solve and update cache
  solve <- function(...) {
    # Check if inverse not calculated
    if (is.null(inv)) {
      # Calculate inverse (assuming invertibility)
      inv <- solve(x, ...)
      m <- x  # Update stored matrix
    }
    
    # Return cached inverse
    return(inv)
  }
  
  # Return a list with the solve function
  list(solve = solve)
}

# cacheSolve computes the inverse of the special matrix from makeCacheMatrix
cacheSolve <- function(x, ...) {
  # Get the solve function from the cached object
  solve <- x$solve
  
  # Call the solve function with arguments
  solve(...)
}

# Example usage
# Create a cached matrix object
myMatrix <- matrix(c(1, 2, 3, 4), nrow = 2)
cachedMatrix <- makeCacheMatrix(myMatrix)

# Solve using the cached inverse (first call will calculate, subsequent calls will use cache)
inverse1 <- cacheSolve(cachedMatrix)
inverse2 <- cacheSolve(cachedMatrix)

# Check if the cached inverse is used for the second call
cat("Are inverse1 and inverse2 the same object? ", identical(inverse1, inverse2))
