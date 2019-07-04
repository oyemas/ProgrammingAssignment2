## this function computes the inverse of a matrix using the solve
## functions in R. first it checks to see if the inverse precomputed, otherwise
## it computes the inverse of the supplied matrix and returns its value

## makecacheMatrix takes in a matrix as input
## imbedded are member functins set,get setinv and getinv
## these member functions are used to set and get the input matrix and its inverse
## respectively

makeCacheMatrix <- function(x = matrix()) {
  ## initialize minv to null meaning the inverse has not been computed
  minv<-NULL
  ##______________________
  
  set<-function(mat){
    ## perform superassignment 
    x<<-mat
    minv<<-NULL}
  ##______________________
  get<-function(){
  x}
  ##______________________
  setinv<-function(mv) {
    minv<<-mv
  }
  ##______________________
  getinv<-function(){
  minv
  }
  ## embed member functions in a list object
  ## this ensures accessibility when instantiated
  
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## givn an instance of makeCacheMatrix this function
## test to see if the inverse of the matrix exist,
## if not it computes it and returns the inverse

cacheSolve <- function(x, ...) {
 ## Return a matrix that is the inverse of 'x'
  ##------ get precomputed inverse if not compute it 
  minv<-x$getinv()
  if(!is.null(minv)){
  return(minv)
  }
  ## get them matrix
  mat<-x$get()
  ## compute the inverse
  minv<-solve(mat)
  ## assign the value
  x$setinv(minv)
  ## return the value
  return(minv)
  
  
}
