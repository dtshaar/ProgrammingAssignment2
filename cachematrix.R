
## Coursera R ProgrammingAssignment2
## dtshaar 26SEP2015

## This code cache's the inverse of a matrix

		

## makeCacheMatrix creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    set <- function(y = matrix()){
        x <<- y
        inv <<- NULL
    }

   
    get <- function(){
        x
    }

    setinv <- function(i){
        inv <<- i
    }

    getinv <- function(){
        inv
    }

    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve computes the inverse of the special "matrix" object
## that is returned by makeCacheMatrix above

        ## if the inverse has already been calculated 
        ## and the matrix has not changed 
        ## then cachesolve retrieves the inverse from the cache 

cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'

    inv <- x$getinv()

    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv

}


        

