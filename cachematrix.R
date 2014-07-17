## These two functions make a matrix, computes and caches its inverse.

## makeCacheMatrix makes a matrix and set the inverse of this matrix as NULL.

makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL;
        set <- function(y) {
                x <<- y;
                iv <<- NULL;
        }
        get <- function() x
        setinv <- function(r) iv <<- r
        getinv <- function() iv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        

}


## cacheSolve takes the output of makeCacheMatrix (which is a list) and checks if the inverse of the matrix
## is NULL. If it is not NULL, return the inverse of the matrix, otherwise compute the inverse and returen it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iv <- x$getinv()
        if(!is.null(iv)) {
                message("getting cached reverse")
                return(iv)
        }
        
        
        i <- solve(x$get())
        x$setinv(i)
        return(x$getinv())
}
