## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#Calculate the inverse of a matrix and cache the result to pull again if needed.


makeCacheMatrix <- function(x=matrix()) {
  inverse2<-NULL
  set<-function(y){
    x<<-y
    inverse2<<-NULL
  }
  get<-function() x
  setInverse<-function(i) inverse2<<- i
  getInverse<- function() inverse2
  list(set=set,
       get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
#Pull the inverse.
#If the inverse is time intensive, pulling from a cache is ideal instead of computing again.


cacheSolve <- function(x, ...){
  inverse2<-x$getInverse()
  if(!is.null(inverse2)) {
    messasge("getting cached data")
    return(inverse2)
  }
  data<-x$get()
  inverse2<-solve(data, ...)
  x$setInverse(inverse2)
  inverse2
}
