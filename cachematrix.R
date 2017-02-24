## This files contains two functions that enable to solve a square inversible matrix
## and to cache the inverse matrix if it has already been calculated.
## The two functions are to be used together to solve the matrix.

## makeCacheMatrix create an object containing a matrix and several functions enabling
## to get or set the value of a matrix and to get or set its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<-y
    inv<<-NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv<<- inverse
  get_inverse<- function() inv
  
  list(set=set,
       get=get,
       set_inverse=set_inverse,
       get_inverse=get_inverse)
}


## cacheSolve takes as argument an object created with makeCacheMatrix
## It first checks if the inverse has already been stored in the input object
## and calculates it if it wasn't.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$get_inverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$set_inverse(inv)
  inv
}
