# Computing and caching the inverse matrix in R.

# The objective is to avoid to compute several times the same object, in this case an inverse matrix.
# This allows to save computational time. The implementation is based on 2 steps:
# 1- Creating the cache and storing the matrix to be inverted.
# 2- Computing the inverse matrix and storing it in the cache.


##  STEP 1: makeCacheMatrix

##  There are 4 functions:

## set() to store the matrix to be inverted
## get() to visualize the matrix to be inverted
## setinv() to store the inverse matrix computed in the second step
## getinv() to visualize the inverse matrix computed in the second step


makeCacheMatrix <- function(x = matrix()) {  
        
        #square matrix as input
        if(dim(x)[1]!=dim(x)[2]) stop("This is not a square matrix")
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## STEP 2: cacheSolve

## Computing the inverse and storing it into the cache:
## the computation is needed only if new data are set up;
## otherwise the function will take the inverse matrix 
## already stored in the cache. 
## This mechanism makes one save computational time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}


## example

a<-diag(5)+.5 #original matrix

M<-makeCacheMatrix(a)   #storing "a" in the cache via set() function
M$get()                 #visualizing the element saved in the cache..
M$getinv()              #it gives a NULL because there is no inverse matrix yet!

cacheSolve(M)           #that gives the inverse matrix and stores it into the cache via setinv() function
M$getinv()              #now you can visualize the inverse matrix
