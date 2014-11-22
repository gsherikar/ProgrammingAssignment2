## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Author: Girish Sherikar
## makeCacheMatrix prepares an object to cache matrix with matrix passed as initial value. Optionally
## logSteps argument may be used to print results from intermediate steps

## Methods available:
## Further it provides methods:
## initCM - erases cached matrix
## initCMI - erases cached inverse
## getCM - returns the cached matrix
## cacheMatrix - caches a new matrix if it is different to the one already cached
## getCMI - returns the inverse of cached matrix
## cacheInverse - Computes new inverse if not already computed 
## setProcTime - captures the processing time to compute inverse
## getProcTime - returns the processing time to retrun the inverse of matrix.

## Credits:
## Following site gave me code to generate an invertible matrix and to idea to capture processing time
## http://stackoverflow.com/questions/19106015/r-how-to-generate-random-yet-easily-invertible-matrices 
## 
## Tweaked and used following to generate invertible matrix for testing
############# set.seed(123)
############# I<-3
############# n<-2000
############# time<-matrix(NA,ncol=3,nrow=I)
############# for(i in 1:I){
#############         t0<-proc.time()
#############         m<-matrix(runif(n^2),n)
#############         x<-solve(matrix(runif(n^2),n))
#############         mt1<-proc.time()
#############          time[i,]<-(mt1-t0)[1:3]
############# }

makeCacheMatrix <- function(x = matrix(),logSteps=FALSE) {

        CM <- x                 # Cached matrix
        CMInverse <- NULL       # Cached Inverse
        procTime <- NULL        # processing time
        
        initCM <- function() {
                
                if (logSteps) {print("Erasing Cached matrix")}
                CM<<-NULL 
        }
        
        initCMI <- function() {
                if (logSteps) print("Erasing Cached Inverse")
                CMInverse <<- NULL
        }
        
        getCM <- function() {
                if (logSteps) print("retrieving cached matrix")
                        return(CM) 
        }
        
        cacheMatrix <- function(x = matrix()) {
                if (!identical(CM,x)) {
                        if (logSteps) print("Caching new matrix")
                        CM <<- x
                }
                else {if (logSteps) print("matrix already in cache")}
        }

        getCMI <- function() {
                if (logSteps) print("returning cached inverse")
                return(CMInverse)
        }
        
        cacheInverse <- function() {
                if (!is.null(CMInverse)) {
                        if (logSteps) print("inverse already cached")
                 }
                 else {
                         if (logSteps) print("computing inverse...please wait")
                         st<-proc.time()        # Start time
                         CMInverse <<- solve(x)
                         et<-proc.time()        # End time
                         diff <-(et-st)[1:3]
                         procTime <<-diff
                 }
        }
        
        setProcTime<-function(tt) {procTime <<- tt}     #Store processing time

        getProcTime<-function() {procTime}
        
        list(initCM=initCM,initCMI=initCMI,getCM=getCM, cacheMatrix=cacheMatrix,
             getCMI=getCMI,cacheInverse=cacheInverse,getProcTime=getProcTime,setProcTime=setProcTime)
        

}


## Write a short comment describing this function
## cacheSolve - returns the inverse of matrix in makeCacheMatrix object. Optional logSteps argument, when TRUE
## prints the intermediate steps as well total processing time to return the inverse - gives an idea
## about benefits of caching. 

cacheSolve <- function(x=makeCacheMatrix,logSteps=FALSE,...) {
        ## Return a matrix that is the inverse of 'x'

        st<-proc.time()
        
        CM <- x$getCM()               # get cached matrix
        
        if (is.null(CM)) {              # if not matrix was cached earlier then cache now and retreieve
                x$cacheMatrix(x)
                CM <- x$getCM()
        }

        #CMInverse <- x$getCMI()       # get Cached Inverse
         
        if (is.null(x$CMI)) { x$cacheInverse() }
        
        et<-proc.time()
        diff <- (et-st)[1:3]
        x$setProcTime(diff)
        if (logSteps) print(diff)
        x$getCMI()
}
