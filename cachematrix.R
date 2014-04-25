# The functions makeCacheMatrix and cacheSolve
# allow for the caching of InverseMatrix, 
# the Inverse of a square invertible matrix

# ----------------------------------------------------------
# function makeCacheMatrix(x)
# x : square invertible matrix() of undefined dimensions
# makeCacheMatrix(x) creates a list of 4 functions
# set: sets the matrix externally
# get: returns the matrix
# setInverseMatrix: sets the inverse matrix externally
# getInverseMatrix: returns the inverse matrix
# ----------------------------------------------------------
makeCacheMatrix <- function(x=matrix()) {
              InverseMatrix <- NULL
	      set <- function(y) {
                  x <<- y
		  InverseMatrix <<- NULL 
              }
              get <- function () x
	      setInverseMatrix <- function(IM) {
                      InverseMatrix <<- IM
		      #print(InverseMatrix)
              }  
	      getInverseMatrix <- function() InverseMatrix
              #returns the following list
	      list (set = set, get = get, 
                    setInverseMatrix = setInverseMatrix,
		    getInverseMatrix = getInverseMatrix)
}


# ----------------------------------------------------------
# function cacheSolve(x,...)
# x : is an object (list) created by makeCacheMatrix 
# cacheSolve(x,...) calculates the InverseMatrix on the first call 
# or retrieves its value if it has been previously found
# ----------------------------------------------------------

cacheSolve <- function(x,...) {
    InverseMatrix <- x$getInverseMatrix()
    if(!is.null(InverseMatrix)) {
        message("getting cached data")
	 return(InverseMatrix)
    }
    matrix <- x$get()
    if(nrow(matrix) != ncol(matrix)) {     
        message <- paste("matrix is not invertible")
        stop (message)
    }
    dimension <- nrow(matrix)
    Identity <- diag(dimension)
    #calculates InverseMatrix here for the first time
    InverseMatrix <- solve(matrix,Identity)
    x$setInverseMatrix(InverseMatrix)
    InverseMatrix
}


#----------------------------------------------------------

