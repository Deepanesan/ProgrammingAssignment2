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

  