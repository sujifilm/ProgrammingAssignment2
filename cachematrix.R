## Matrix inversion caches the inverse of a matrix.

## Creates the inverse of a special matrix cache

makeCacheMatrix <- function(x = matrix()) {
inv<-NULL #to set inverse as NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x #to get matrix x
  setinv <- function(inverse) {inv <<- inverse}
  getinv <- function() {
    inver<-ginv(x)
    inver%*%x #to get inverse of matrix
  }
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Computes the inverse of the matrix and retrieves the inverse

cacheSolve <- function(x, ...) {
       inv<-x$getinv()
  if(!is.null(inv)){ #to check if inverse is NULL
    message("getting cached data.")
    return(inv) #to return inverse value
  }
  data<-x$get()
  inv<-solve(data, ...) #to solve inverse value
  x$setinv(inv)
  inv
}
#End
