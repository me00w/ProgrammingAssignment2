## Two functions: makeCacheMatix and cacheSolve to cache the inverse of a matrix, 
##and to calculate the inverse of a matrix


## makeCacheMatrix takes your matrix as an input and will cache the inverse of the
##matrix (does not actually calculate the inverse)
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                            #this stores the inverse of the matrix x
        
        set <- function(y) {                 #caching y so that when x changes the inverse can be updated
                x <<- y
                i <<- NULL                   #when x changes NULL the cached inverse
        }
        
        get <- function() {
                x
        }
        
        setinverse <- function(inverse) {    #caches inverse of the matrix
                i <<- inverse
        }
        
        getinverse <- function() {           #returns the inverse of the matrix
                i
        }

        list(set = set, get = get,           #returns all these functions as a list
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve will take a cached matrix, check to see if its inverse is cached,
## then calculate the inverse if it is not cached
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")  ##this slows your function down, so if replicating the function it should be removed
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

##How this works:
##Define a matrix (let's call it c)
##Call makeCacheMatrix(c) and set it to another variable (let's call it k), so now k knows what c is
##Call cacheSolve(k) because k is our cached matrix, c isn't cached
##cacheSolve(k) will return the inverse of k, either by getting the cache or solving it new and caching it
