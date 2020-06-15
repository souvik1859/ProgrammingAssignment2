## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Calculating the inverse of a matrix through caching.
## Calculation of inverse of matrix is costly computation and it is beneficial if we use caching method 
## rather than compute it repeatedly.
## A pair of functions that are used to create a special object that stores matrix and caches the inverse.
makeCacheMatrix <- function(x = matrix()) {


        n<-NULL
        set<-function(z){
                x<<-z
                n<<-NULL
        }
        get<-function() x
        setInverse<-function(inverse) n<<-inverse
        getInverse<-function() n
        list(set=set,get=get,
        setInverse=setInverse,
        getInverse=getInverse)
}


## Write a short comment describing this function

## This function computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed, then it should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n<-x$getInverse()
        if(!is.null(n)){
                message("getting cached data")
                return(n)
        }
        data<-x$get()
        n<-solve(data)
        x$setInverse(n)
        n
}
