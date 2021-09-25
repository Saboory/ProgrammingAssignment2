
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        
        inv<-NULL                  #initializing inverse as Null
        set <- function(y){
                x <<- y
                Inv <<- NULL
        }
        get<- function(){x}        #Function to get Matrix x
        setInverse<-function(inverse){inv<<-inverse}
        getInverse<-function(){
                inver<-ginv(x)
                inver%*%x       #Function to obtain inverse of the matrix
        }
        
        list(set=set,
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
        
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)){
                message("getting inversed matrix!")
                return(inv)
        }
        data<-x$get()
        inv<-solve(data,...)
        x$setInverse(inv)
        inv
}

################### Example to test  #######################
##      f<-makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,5,0) , 3 , 3))

##      f$get()

##      cacheSolve(f)