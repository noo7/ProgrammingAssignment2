## Functions to create Matrices and access the inverse of Matrices
## while caching the inverse


## This function return a special matrix containing
## an element storing its own inverse, plus a few functions
## to set and get the data it contains

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the inverse to null
    inv <- NULL
    
    ## set the data to be equal to y
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## get the contents of the matrix
    get <- function() x
    ## set the Inverse of the matrix
    setInv <- function(inverse) inv <<- inverse
    ## get the inverse of the matrix
    getInv <- function() inv
    
    #return a list conatining accessors to the data
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}


## This function returns the inverse of a matrix x
## if the inverse of the matrix has alredy been calculated
## and its contents havent been changed since last calculation
## then the cached answer to the inverse will be returned

cacheSolve <- function(x) {
    ## get the Inverse from x which is cached in x
    inv <- x$getInv()
    ## check if retrievedvalue is set, if so return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## retrived cached inverse is not set, so calculate it, set it, return it
    data <- x$get()
    inv <- solve(data)
    x$setInv(inv)
    inv
}
