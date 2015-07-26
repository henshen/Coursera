## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix that caches its inverse, as inverse 
## computation is taxing from a systems perspective


makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setInv<- function(s) invrs <<- s
        getInv <- function() invrs
        list(set = set, get = get,
             setInv = setInv, getInv = getInv)
}

## The following function computes the inverse of the matrix created by makeCasheMatrix 
## If the inverse has already been calculated,it will retrieve the inverse from 
## the cache and skips computation; else, it computes the inverse and stores the 
## value in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getInv()
        if (!is.null(invrs)) {
                message("cache available, retrive data")
                return(invrs)
        }
        mat <- x$get()
        invrs <- solve(mat, ...)
        x$setInv(invrs)
        invrs
}

