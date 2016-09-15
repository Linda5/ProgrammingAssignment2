## This function creates a special "matrix" object that can cache its inverse.
## 


makeCacheMatrix <- function(x = matrix()) {
    m<- NULL  ## initialising m
    set<-function(y) {
        x<<-y
        m<<-NULL
    }
    get<-function()x                        ## defines the getter for the matrix x
    setinverse<-function(solve) m<<-solve   ## defines the setter for the inverse m
    getinverse<-function() m                ## defines the getter for the inverse m
    ##  Returns a fully formed object of type makeCacheMatrix.  All the elements are named.
    list(set=set,   
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.  
##  If the inverse has already been calculated (and the matrix has not changed), then the 
##  cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m<-x$getinverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data<-x$get()
    m<-solve(data,...)
    x$setinverse(m)
    m
}
## Create Test Matrix
TestMatrix<-matrix(c(2,4,6,8),nrow=2,ncol=2)
aResult<-makeCacheMatrix(TestMatrix)
aResult
bResult<-cacheSolve(aResult)
bResult
## Rerun to see if it uses cached results.
bResult<-cacheSolve(aResult)
bResult



