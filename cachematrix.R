## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
  i <- NULL                                 
  set <- function(y) {                      
    x <<- y                                 
    i <<- NULL }                            
  get <- function() x                       
    SetInv <- function(inverse) i <<- inv   
    GetInv <- function()i                   
  
  list(set = set, get = get, SetInv = SetInv, GetInv = GetInv) 

}


## This function generates inverse of matrix, by retrieving the cache of the above function.

cacheSolve <- function(x, ...) {
    i <- x$GetInv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$SetInv(i)
    i
}
