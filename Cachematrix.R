##Catching the inverse of Matrix 
##created Null matrix to store the list of matrix
library(MASS)

makeCacheMatrix<- function(x=matrix()){
  inv<-NULL
  set<-function(y){
                  x<<-y
                  inv<<-NULL
  }
  get<-function()x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function(){
                    inver<-ginv(x)
                    inver%*%x              #function to obtain inverse of the matrix
  }
  
  list(set=set,get=get,
       setinv = setinv,
       getinv = getinv)
}


##Cachesolve created to compute the inverse of the matrix

cachesolve<- function(x,...) #gets cache data
{
  inv<-x$getinv()
  if(!is.null(inv)){
                   message("getting cached data!")
                  return(inv)       #returns inverse value
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv    
}

f<-makeCacheMatrix(matrix(1:8,2,4))
f$get()
f$getinv()  

cachesolve(f)
