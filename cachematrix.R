## Put comments here that give an overall description of what your
## functions do
#
# The makeCacheMatrix() function is a constructor function, taking a matrix parameter 
# and storing this matrix, along with a variable to hold the inverse of the matrix, 
# and a set of accessor functions, which are rerturned as a list to make the functions 
# available to the cacheSolve() function.
#
# The cacheSolve function returns the cached value of the matrix inverse. If the 
# cached inverse is null, the function utilises the accessor functions returned by
# the makeCacheMatrix function to retrieve the matrix (getMatrix()), calculate the 
# inverse (solve()), assign the inverse to the cachedInverse variable in the defining 
# function(setInverse()) and return the inverse.


## Write a short comment describing this function
#
# Within the makeCacheMatrix() function are defined: a variable to store the inverse 
# of the matrix passed to the function, get & set accessors for the matrix, get & set 
# accessors for the matrix inverse. The function returns a list of the four accessor functions. 
# The '<<-' operator is used in the set accessor functions to assign the given value
# to the cachedInverse variable in the defining environment (rather than assigning to a 
# newly created cachedInverse variable within the set accessor function).


makeCacheMatrix <- function(x = matrix()) {
    # create cachedInverse variable; assign NULL value
    cachedInverse <- NULL
    
    # note: this function is not called for this exercise
    setMtrx <- function(anotherMatrix) {
        # new matrix introduced
        x <<- anotherMatrix
        
        # cached inverse variable is reassigned to NULL; 
        # note '<<-' assigns to cachedInverse in the defining (parent) environment
        cachedInverse <<- NULL
    }
    
    # returns matrix
    getMtrx <- function() {
        x
    }
    
    # writes the calculated matrix inverse value returned by cacheSolve() function to
    # the cachedInverse variable (again, note use of '<<-' operator)
    setInv <- function(cachedInv) {
        cachedInverse <<- cachedInv
    }
    
    # returns cached value of matrix inverse (if NULL, indicates first call of 
    # cacheSolve() function)
    getInv <- function() {
        cachedInverse
    }
    
    # returns a list of get & set functions for use by the cacheSolve() function
    list(setMatrix = setMtrx, 
         getMatrix = getMtrx,
         setInverse = setInv,
         getInverse = getInv)
}


## Write a short comment describing this function
#
# The cacheSolve() function utilises the list of accessors returned as list from
# makeCacheMatrix() to return the inverse of the matrix stored in the defining 
# environment of the accessor functions. 
# If the cached inverse of the matrix, retrieved by the getInverse() function, is 
# NULL, indicating the first time accessed, the matrix is retrieved from the 
# defining environment (makeCacheMatrix() function) using the getMatrix() function, 
# the inverse is calculated with solve() and the inverse is assigned to the 
# cachedInverse variable in the defining function using the setInverse() function; 
# the calculated inverse matrix is returned.
# If the cached inverse of the matrix is not NULL, the function will simply return
# the cached matrix inverse value.


cacheSolve <- function(x, ...) {
    # retrieve the value of the cachedInverse from the defining environment 
    # of the getInverse() function
    cachedInverse <- x$getInverse()
    
    if(!is.null(cachedInverse)) {
        # not NULL indicates a cached value; returns this value
        message("Retrieving cached matrix inverse.")
        
        # return the cached matrix inverse value
        return(cachedInverse)
    } else {
        # NULL indicates no cached value; new matrix inverse value will 
        # be calculated        
        message("Calculating matrix inverse.")
        
        # retrieve matrix from the from the defining environment of the 
        # getMatrix() function
        matrix <- x$getMatrix()    
        
        # matrix inverse is calculated and assigned to local cachedInv variable
        cachedInv <- solve(matrix)
        
        # setInverse() function writes the value of cachedInv to 
        # the cachedInverse variable in the function's defining environment
        x$setInverse(cachedInv)
        
        # return the newly cached matrix inverse value
        return(cachedInv)
    }
}