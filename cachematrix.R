## We can create a function "makeCacheMatrix" that allows us to create a matix by specifying 
## the no. of rows and columns, elements, etc by using the function() directive.
## The set function created assigns the value of the argument 'y' to the matrix object 'x'
## using the <<- operator.
## 'inv' is assigned the value 'NULL'. The get() function gets the matrix specified i.e 'x'.
## "setInverse" sets the inverse of the matrix object and "getInverse()" can be used to get the 
## inverse of the matrix.

## We also create cachesolve function to compute the inverse of 'x' and cache it.
## 'inv' specified inside the function, gets the inverse of 'x' and if the inv is not NULL
## it gives a message "getting cached data" and returns the inverse of the matrix.
## "solve" function gives the inverse of the matrix specified as its arguments.


## The fuction makeCacheMatrix is a function that takes the function matrix() 
## as an argument which specifies the elements of the matrix and no. of rows and columns.
## It can be used to create a matrix object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  set<- function(y){
    x<<- y
    inv<<- NULL
  }
  get<- function(){x}
  setInverse<- function(inverse){inv<<- inverse}
  getInverse<- function(){inv}
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## cachesolve function computes the inverse of the matrix returned by makeCacheMatrix.
## It retrieves the inverse if it is already cached provided the matrix is not changed.


cacheSolve <- function(x, ...) {
  inv<- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<- x$get()
  inv<- solve(mat, ...)
  x$setInverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}


## Console looks as below:
## >source("cachematrix.R")
## >vmatrix<- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
## >vmatrix$get()
## Ans is :    [,1] [,2]
##        [1,]    1    3
##        [2,]    2    4
## 
## >vmatrix$getInverse()
## Ans: NULL
## >cachesolve(vmatrix)
## Ans is: [,1] [,2]
##   [1,]   -2  1.5
##   [2,]    1 -0.5
## > cachesolve(vmatrix)
## Ans: getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > vmatrix$getInverse()
## Ans is: [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5