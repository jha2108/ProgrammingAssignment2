makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("Please Wait")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
my_matrix<-makeCacheMatrix(matrix(1:4,2,2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()