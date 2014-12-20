##This function returns a list of functions that helps to get and set the original input matrix. Also, it has getInverse and setInverse functions to cache the inverse matrix once computed
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL #cached inverse stored here.
        set <- function(y) {
                x <<- y #actual data
                inv <<- NULL #initially set to null
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) #returning a vector of functions.
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	invMat <- x$getInverse()
        if(!is.null(invMat)) { #if the inverse is calculated before, then get the cached inverse
                message("getting cached data")
                return(invMat)
        }
        orgMat <- x$get() 
        inverse <- solve(orgMat) #solving for inverse of matrix
        x$setInverse(inverse) #caching the inverse once calculated
        inverse
}

