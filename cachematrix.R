## This script contains of two functions  makeCacheMatrix , cacheSolve 
## to providing a matrix and inverse of that


#------------------------------------------------------------------------------
##      makeCacheMatrix() : this function creates and store a matrix as an object 
##      with SET and GET methods 
#------------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {
        
        inv<-NULL
        set <- function(y){     ## Set input matrix
                x <<- y
                Inv <<- NULL
        }
        get<- function(){x}                             ## Obtaining input  matrix
        setInverse<-function(inverse){inv<<-inverse}    ## Setting inverse matrix
        getInverse<-function(){inv}                     ## Obtaining inverse matrix
        list(set=set,                                   ## Creating result list
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
        
}

#------------------------------------------------------------------------------
##      cacheSolve() : this function computes the Inverse of the special matrix
##      returned by makeCacheMatrix(), 
##      by calling cacheSolve() and the object which comes from  makeCacheMatrix
##      it creates the inverse matrix and store it 
##      after creating the inverse matrix by calling getInverse() we can read 
##      the inversed matrix, it will be there until main matrix has not changed
#------------------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)){      ## Obtaining cached inverse matrix
                message("getting inversed matrix!")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        x$setInverse(inv)
        message("*********      Matrix is inversed      *********")
        inv ## Return the inverse matrix of 'x' and cache it in the memory
}


###################     Example to test         #######################

##      To create a matrix      --->      f<-makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,5,0) , 3 , 3))

##      To see the content of the matrix        --->    f$get()

##      To creating inverted matrix             --->    cacheSolve(f)           

##      To See the inverted Matrix              --->    f$getInverse()

###################             Good Luck         #######################