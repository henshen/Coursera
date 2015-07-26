## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix that caches its inverse, as inverse 
## computation is taxing from a systems perspective


makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL #storing any inversion results in this variable
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        } # setting a matrix to object as created by makeCacheMatrix; also apply null
        # to invrs
        
        get <- function() x #returning input matrix
        setInv<- function(s) invrs <<- s #setting the inverse matrix
        getInv <- function() invrs #returning the inverse matrix
        list(set = set, get = get,
            setInv = setInv, getInv = getInv)
} #returning a list that is inclusive of all above functions, to be used in 
#an object context for the makeCacheMatrix function

## The following function computes the inverse of the matrix created by makeCasheMatrix 
## If the inverse has already been calculated,it will retrieve the inverse from 
## the cache and skips computation; else, it computes the inverse and stores the 
## value in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getInv() #obtaining the inversed matrix from object x
        if (!is.null(invrs)) {
                message("cache available, retrive data")
                return(invrs)
        } #returns Null if no inverse has been calculated; 
          #or else returns the calculated inversion
        mat <- x$get() #if inversion hasn't been calculated, use x$get() to obtain matrix
        invrs <- solve(mat, ...) #calculate the inversion of the matrix
        x$setInv(invrs) #set the inversion to the object 
        invrs #return results
}

