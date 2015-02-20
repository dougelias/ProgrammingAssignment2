## Create a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## initialize object
    m <- NULL

    ## modify objects in external environment
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    ## functions on the object (self-explanatory)
    get <- function() x
    setinv <- function(cacheSolve) m <<- cacheSolve
    getinv <- function() m

    ## what makeCacheMatrix returns
    ## (an anonymous list providing
    ## access to all of the functions
    ## on the object
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

## Calculates the inverse of the
## 'special' matrix created above

cacheinv <- function(x, ...) {
    
        ## Check to see if the inverse has already been
        ## calculated and ... 
        if(!is.null(m)) {
            
            ## ... if so, gets the inverse from the cache ...
            m <- x$getinv()
            message("getting cached data")

            ## and skip the computation
            return(m)
        }
        
        ## ... else calculate the inverse ...
        data <- x$get()
        m <- cacheSolve(data, ...)

        ## ...modify the object in the
        ## external environment, and ...        
        x$setinv(m)

        ## ...return the object
        m
}

## Return the inverse of x

cacheSolve <- function(x, ...) {
    return (solve(x))
}
