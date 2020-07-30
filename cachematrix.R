## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#function makeCacheMatrix creates a list of values: it sets the matrix, 
#gets the matrix, sets the inverted matrix, and gets the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        #sets m to NULL
        m <- NULL
        #sets the global value of x to y, and m to NULL
        setmatrix <- function(y) {
                x <<- y
                m <<- NULL
        }
        #function that gets the value of matrix x
        getmatrix <- function() x
        #function that sets the global value of m to inv (inverse)
        setinv <- function(inv) m <<- inv
        #function that gets the value of inverse of the matrix x
        getinv <- function() m
        list(setmatrix = setmatrix, getmatrix = getmatrix, 
             setinv = setinv, getinv = getinv)
}


#The function checks if the x has already a stored inverted matrix
#if x contains the inverted matrix, it returns the value
#if x does not contain the inverted matrix, the function calculates it
#via the solve function, tores the calculation, and prints it

cacheSolve <- function(x, ...) {
        #checking if there is a non-null value of inverse of the matrix x
        m <- x$getinv()
        if(!is.null(m)){
                message('Checking cached data')
                #there was a non-null value and the function returns it
                return(m)
        }
        #the function set the value of 'data' to the matrix x, stored in getmatix
        data <- x$getmatrix()
        #setting the value of m to the inverted matrix stored in 'data'
        m <- solve(data, ...)
        #setinv value is set to m
        x$setinv(m)
        #returning the inverted matrix
        m
        
}
