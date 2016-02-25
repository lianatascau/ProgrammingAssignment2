makeCacheMatrix <- function(x = matrix()){
        # makeCacheMatrix will create a list containing a function
        # to set, get the value of a matrix, and
        # set, get the value of the inverse of a matrix
        matrix.inverse <- NULL
        set <- function(y){
                x <<- y
                matrix.inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inv) matrix.inverse <<- inv
        getinv <- function() matrix.inverse
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}
cacheSolve <- function(x, ...){
        # calculates the inverse of a matrix
        # returns  a matrix inverse
        # if the matrix inverse is not null
        matrix.inverse <- x$getinv()
        if(!is.null(matrix.inverse)){
                message("getting cached data")
                return(matrix.inverse)
        }
        data <- x$get()
        matrix.inverse <- solve(data, ...)
        x$setinv(matrix.inverse)
        return(matrix.inverse)
}