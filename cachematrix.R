#This set of functions creates a matrix, grabs and stores the inverse, and then later retrieves the inverse if it was stored, or finds it

#make the matrix and the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        #Same as with the vectors, get the inverse
        inv = x$getinv()
        
        #Check to determine if the inverse has been cached
        if (!is.null(inv)){
                #Retrieve the cache
                message("getting cached data")
                return(inv)
        }
        
        #Otherwise solve for the inverse
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        #Lets go ahead and cache the inverse
        x$setinv(inv)
        
        return(inv)
}