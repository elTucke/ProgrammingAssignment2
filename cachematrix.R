## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  cached_inverse <- NULL
  
  # We need accessor methods for the matrix x
  set <- function(matrix_to_assign) {
    x <- matrix_to_assign
    
    # When assigning a new matrix, discard any cached inverse
    # TODO: Maybe we should only do that if the new matrix is
    # different from the old one
    cached_inverse <- NULL
  }
  get <- function() x
  
  # ...and we need accessors for its cached inverse as well
  get_inverse <- function() cached_inverse
  
  set_inverse <- function(inverse_to_cache) cached_inverse <<- inverse_to_cache
  
  list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # The argument 'x' is not a matrix, but a vector (or list??) of
  # four functions: a pair of accessors for the actual matrix we're
  # inversing and a pair of accessors for its cached inverse.
  
  cached_inverse <- x$get_inverse()
  if(!is.null(cached_inverse)) {
    message("getting cached data")
    return(cached_inverse)
  }
  
  # We couldn't find an inverse for x's matrix already cached.
  # We'll need to get the matrix originally given to makeCacheMatrix
  # and use R's solve() on it
  full_matrix <- x$get()
  inverse <- solve(full_matrix)
  # Getting that inverse matrix was expensive. Let's cache it!
  x$set_inverse(inverse)
  
  inverse
}