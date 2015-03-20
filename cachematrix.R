## These two functions cache and solve for the inverse of matricies. The First function
## caches the matrices and the second will check if the matrix has been solved and solve it
## if it is not already solved.

## This function creates a cache (store) of matricies that can be called up and used by the
## second function.

makeCacheMatrix <- function(x = matrix()) {

  m<- NULL

  set <- function(y){
    x <<-y
    m<<-NULL
  }
  get<- function() x
  setMatrix <- function(solve) m <<-solve
  getMatrix <- function() m
  list(set=set, get=get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}


## This Function checks to see if an inverse of 'x' has already been calculated.
## If it has the function will display the inverse of 'x', and if not it will solve for 
## the inverse of 'x' and then display it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <-x$getMatrix()
  if(!is.null(m)){
    message("getting cashed data")
    return(m)
  }
  matrix <-x$get()
  m <-solve(matrix,...)
  x$setMatrix(m)
  m
}
