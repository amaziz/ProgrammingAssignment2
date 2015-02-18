# This code calculates the inverse of an invertible matrix
# It consists of 2 functions. 
# Warning! Code doesn't check whether a matrix is invertible.
# It just assumes that it is.
#
# Example test case:
# testmatrix = matrix(c(2,2,3,2), nrow=2, ncol=2)
# preparedmatrix <- makeCacheMatrix(testmatrix)
# cacheSolve(preparedmatrix)
# cacheSolve(preparedmatrix)


## This function gets and sets the matrix to be evaluated (called x)
## The function also gets and set the invese of the matrix

makeCacheMatrix <- function(x = matrix()) {
        matrixinverse <- NULL
        set <- function(y) {
                x <<- y
                matrixinverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) matrixinverse <<- inverse
        getinverse <- function() matrixinverse
        matrix(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function checks if an inverse is calculated
## If not, it calculates the inverse using the solve() function
## It thn sets the matrixinverse accirdingly and returns its value

cacheSolve <- function(x, ...) {
        matrixinverse <- x$getinverse()
        if(!is.null(matrixinverse)) {
                message("getting cached data")
                return(matrixinverse)
        }
        data <- x$get()
        matrixinverse <- solve(data)
        x$setinverse(matrixinverse)
        
        ## Return a matrix that is the inverse of 'x'
        matrixinverse
}

