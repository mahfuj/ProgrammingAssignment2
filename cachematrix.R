# Example usage:
# > x <- matrix(rnorm(25), nrow = 5)          // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create our special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call the 2nd time, so return
#                                             // the cached inverse

# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix - set()
# 2. Get the value of the matrix - get()
# 3. Set the value of the inverse - setInverseMatrix()
# 4. Get the value of the inverse - getInverseMatrix()

makeCacheMatrix <- function(x = matrix()) 
{
  # inverseMatrix will store the cached inverse matrix
  inverseMatrix <- NULL
  
  # Set the matrix
  set <- function(y) 
  {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  # Get the matrix
  get <- function() 
  {
    x
  }
  
  # Set the inverse matrix
  setInverseMatrix <- function(inverseMatrixSet) 
  {
    inverseMatrix <<- inverseMatrixSet
  }  
    
  # Get the inverse matrix
  getInverseMatrix <- function() 
  {
    inverseMatrix
  }
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get, 
       setInverseMatrix = setInverseMatrix, 
       getInverseMatrix = getInverseMatrix)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverseMatrix()
  
  # If the inverse is already calculated, return it
  if (!is.null(inverseMatrix)) 
  {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  # The inverse is not yet calculated, so we calculate it
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  
  # Cache the inverse
  x$setInverseMatrix(inverseMatrix)
  
  # Return it
  inverseMatrix
  
}
