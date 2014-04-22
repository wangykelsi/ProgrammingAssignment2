## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing a funcion to
## 1.set - set the value to the matrix
## 2.get - get the value of the matrix
## 3.setinv - set the value of the inverse matrix
## 4.getinv - get the cached value of inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse)  inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: caculates the inverse matrix of the special matrix, 
## which follows the steps:
## 1.get the cached inverse matrix
## 2.if the cached inverse matrix is not null, return it. 
##   else goto step3
## 3.caculate the inverse matrix 
## 4.set the result of step 3 to the special matrix
## 5.return the result
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
