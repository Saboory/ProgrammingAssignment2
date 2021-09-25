## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        inv<-NULL
        set <- function(y){     ## Set input matrix
                x <<- y
                Inv <<- NULL
        }
        get<- function(){x}     ## Obtaining input matrix
        setInverse<-function(inverse){inv<<-inverse}    ## Setting inverse matrix
        getInverse<-function(){inv}      ## Obtaining inverse matrix
        list(set=set,                 ## Creating result list
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
        
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getInverse()
        if(!is.null(inv)){      ## Obtaining cached data
                message("getting inversed matrix!")
                return(inv)
        }
        mat<-x$get()
        inv<-solve(mat,...)
        message("calculating ... ")
        x$setInverse(inv)
        message("*********      Matrix is inversed      *********")
        invisible( inv)## Return a matrix that is the inverse of 'x'
}


################### Example to test  #######################

##      To create a matrix      --->      f<-makeCacheMatrix(matrix(c(1,0,5,2,1,6,3,5,0) , 3 , 3))

##      To see the content of the matrix        --->    f$get()

##      To creating inverted matrix             --->    cacheSolve(f)           

##      To See the inverted Matrix              --->    f$getInverse()

################### Good Luck  #######################