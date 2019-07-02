## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  minv<-NULL
  set<-function(mat){
    x<-mat
    minv<-NULL}
  get<-function()
  {x}
  setinv<-function(mv) 
  {minv<<-mv}
  getinv<-function()
  {minv}
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv<-x$getinv()
  if(!is.null(minv))
  {return(minv)
  }
  mat<-x$get()
  minv<-solve(mat)
  x$setinv(minv)
  return(minv)
  
  
}
