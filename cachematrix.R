## INVERSE OF A MATRIX LEXICAL SCOPING.
##
## AUTHOR: Pranav Joshi
## DATE: 18/01/2016

## EXAMPLE MATRIX.
##orig_matrix <- matrix (c(1,0,5,2,1,6,3,4,0), nrow = 3, ncol = 3)
##orig_matrix <- matrix (c(1,1,4,0,3,1,4,4,0), nrow = 3, ncol = 3)

##     EXECUTION STATEMENTS      ##

## mat <- matrix (c(1,1,4,0,3,1,4,4,0), nrow = 3, ncol = 3)
## m1<-makeCacheMatrix(mat)
## cacheSolve(m1)

# makeCacheMatrix is a function that returns a list of functions
# It stores a martix and a cached value of the inverse of the 
# matrix. Contains the following functions:
# * set      set the value of a matrix
# * get      get the value of a matrix
# * setInv   set inverse of the matrix
# * getInv   get inverse of the matrix

makeCacheMatrix <- function (x = matrix ())
{
        cache_inv <- NULL
        set <- function (y)
        {
                x <<- y
                cache_inv <- NULL        
        }
        get <- function()x
        setinv <- function(solve) cache_inv <<- solve
        getinv <- function() cache_inv
        list (set = set, get = get, getinv = getinv, setinv = setinv)
}

# This function performs the inverse of a "special" matrix created using makeCacheMatrix.

cacheSolve <- function (x,...)
{
        cache_inv <- x$getinv()
        if(!is.null(cache_inv)) {
                print("getting cached data")
                return(cache_inv)
        }       
        
        data <- x$get()
        inv <- solve (data, ...)
        x$setinv (inv)
        inv
        
}