## Functions that cache the inverse of a matrix

## Creates a matrix that caches the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(matrix) {
      x<<-y
      inv<<-NULL
}
  get<-function()x
  setInverse<-function(inverse)inv<<-inverse
  getinv<-function(){
                    inver<-ginv(x)
                    inver%*%x
                    }
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)
}

## Function that receives cached data

cacheSolve <- function(x, ...) {
  inv<-x$getinv()
  if(!is.null(inv)){
              message("getting cached data!")
              return(inv)
  }
  dat<-x$get()
  inv<-solve(data...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
