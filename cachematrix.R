## We define two functions:
## 1. makeCacheMatrix, which effectively creates 
## a makeCacheMatrix object from a matrix object, 
## which will encapsulate this original matrix 
## together with its inverse

## 2. cacheSolve, which retrieves this inverse 
## from inside the makeCacheMatrix object, 
## provided that it has already been calculated, 
## otherwise calculates the inverse and stores it
## inside the object in case we might need it again
## sometime in the future


## So, the first function creates an object 
## in which are embedded:
## 1. the original matrix 'x'
## 2. its inverse 'i'
## These two matrices are not directly accessible 
## or modifiable, but are accessed and modified through 
## the corresponding get and set functions, 
## and the makeCacheMatrix object is effectively 
## constructed as a list of these functions

makeCacheMatrix <- function(x = matrix()) {
    
    ## There are basically two embedded entities inside a makeCacheMatrix object:
    ## 1. the original matrix, 'x'
    ## 2. the inverse of this matrix, 'i'
    
    ## When a makeCacheMatrix object is constructed for the first time, 
    ## the original matrix is passed as an argument to the constructor
    ## As for the inverse, we set the default value of the inverse to be NULL
    i <- NULL 
    
    ## We define a set function, which enables us:
    ## 1. to set the embedded matrix inside the makeCacheMatrix object to be equal 
    ## to some given matrix, which is passed as the argument to this function
    ## 2. to set the cached value of the inverse to be NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## This function enables us to retrieve to embedded starting matrix
    ## inside the makeCacheMatrix object, given a reference to this object
    get <- function() x
    
    ## This function enables us to set the cached value of the inverse, 
    ## once we calculate it
    setInverse <- function(inverse) i <<- inverse
    
    ## This function enables us to retrieve the cached value of the inverse, 
    ## which may be NULL, if we haven't calculated it yet
    getInverse <- function() i
    
    ## The makeCacheObject will consist of a list of functions, using which
    ## 1. we can retrieve the embedded original matrix, 
    ## 2. set it to be equal to another matrix, 
    ## 3. get the cached inverse matrix if it has already been calculated,
    ## 4. cache the inverse matrix, the first time we calculate it
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The second function does the following:
## 1. takes a makeCacheMatrix object, which contains in itself 
## a matrix 'm' and its inverse 'i', and retrieves this inverse, 
## if it has already been calculated

## 2. otherwise, calculates the inverse of the matrix 'm' 
## and stores it inside the makeCacheMatrix object 
## which has been passed to it as an argument

## In either case, it returns the value of the inverse, 
## that is, provided that the matrix 'm' is invertible!

cacheSolve <- function(x, ...) {

    ## First, we try to retrieve the inverse matrix from inside the object
    i <- x$getInverse()
    
    ## If 'i' is not NULL, i.e. the inverse has already been calculated, 
    ## we return the inverse matrix and we are done
    if (!is.null(i)) {
        message("getting cached data")
        i
    }
    
    ## Otherwise, we need to retrieve the original matrix,
    ## then use the solve function to calculate the inverse of this matrix, 
    ## and then return this inverse matrix, but before that we store it
    ## inside the makeCacheMatrix object for future reference
    else {
        m <- x$get()
        i <- solve(m)
        x$setInverse(i)
        i
    }
}
