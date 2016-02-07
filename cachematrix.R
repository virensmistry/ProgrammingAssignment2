##Initializing the function and predefining the size of the matrix to a square matrix for easy operation.
## Using the code provided to find the mean I developed this code to find the inverse matrix

makeCacheMatrix<- function(x=matrix(, n.rows=3, n.col=3))
{
      mat<- NULL #initializing matrix to null.
      setmatrix <- function(y) 
      {
            x <<- y
            mat <<- NULL #
      }
      getmatrix <- function() x
      setinverse <- function(solve) mat <<- solve
      getinverse <- function() mat
      list(setmatrix = setmatrix, getmatrix = getmatrix, 
           setinverse = setinverse, getinverse = getinverse)
}

#Inversing the matrix if the same matrix is found in the cache using the below function.

cacheSolve <- function(x, ...) 
{
  mat <- x$getinverse()
  if(!is.null(mat)) 
  {
    message("getting cached data")
    return(mat)
  }
  data <- x$getmatrix()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}
