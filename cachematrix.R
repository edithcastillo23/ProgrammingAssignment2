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
# Using function makeCacheMatrix(x)
#----------------------------------------------------------

#x <- matrix(c(4,3,3,2), nrow=2, ncol=2)
##dimension_x = nrow(x)
##identity_matrix = diag(dimension_x)
##nrow(x)
##ncol(x)
##solve(x,identity_matrix)

#a <- makeCacheMatrix(x)    #creates a list of 4 functions
##print(x)
##class(x)
##print(a)    #will print the list
##class(a)    #returns that a is a list

##To call the functions in the list:
##class(a$get)  #will print that a$get is a function
##a$get()       #returns matrix created above
##a$set(Identity)  #setting the matrix with the $set function
##a$get()
##a$setInverseMatrix(8)   #could set the InverseMatrix here
##a$getInverseMatrix()
##note you can write a number 'cause type wasn't especified

#----------------------------------------------------------
# Using function cacheSolve
#----------------------------------------------------------

#b <- cacheSolve(a)    #first call, IS the InverseMatrix
      		      #calculated for the first time
##class(b)             #returns that b is a matrix
#b

#b <- cacheSolve(a)    #InverseMatrix calculated before, will
     		     #return value previously calculated
#b

