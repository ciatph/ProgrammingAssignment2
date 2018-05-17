## Programming Assignment: Lexical Scoping 
## For JHU Data Science - R Programming course
## madbarua 20180517

## Creates a special "matrix" object that can cache its inverse
## The matrix supplied is always invertible
## setinverse: solve and cache the matrix inverse
## getinverse: get the cached matrix inverse
## get: get the original matrix

makeCacheMatrix <- function(m = matrix(c(1,2,3,1), 2, 2)){
  inv <- NULL
  set <- function(y){
    m <<- y
	inv <<- NULL
  }    
  get <- function() m
  setinverse <- function(){
    inv <<- solve(m)
  }
  getinverse <- function() inv
  list(
       set = set,
       get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## Attempts to return "cached" inverse matrix if its already pre-computed
## Computes the inverse matrix if its not yet cached 

cacheSolve <- function(x, ...){
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  solve(x$get())
}