## The goal is to save Matrix inverse computation time, which can be a costly operation for large matrices
## This is achieved with the help of two functions  
## Function makeCacheMatrix() returns a cached Matrix object 
##			Return value is a list that provides methods to access matrix and its cached inverse, once it has been computed.
## Function cacheSolve() returns the inverse of the cached matrix object.
##			Return value is retrieved from cache, if the inverse has been already computed once. 
## Sample usage-
## 	mat <- matrix(1:4,2,2)   # make a matrix
##  matrixObject <- makeCacheMatrix(mat)  # run function to create an object to hold the cached data
##  cacheSolve(matrixObject)      # returns inverse. uses cached value if available; if not, it caches it for future;


## makeCacheMatrix(x=matrix()) 
##		- x: input argument x is a matrix 
##  	- Return value is a list that contains following functions
##				---set: 		allows setting of a new matrix 
##				---get: 		retrieves the cached matrix
##				---setinverse: 	sets inverse of the matrix for caching
##				---getinverse: 	gets cached inverse of the matrix if available.
makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL 					# inv stores the inverse of the matrix and is initialized to null.
		
		## set allows setting of a new matrix
		set <- function(y) {			
				x <<- y					# y is the new matrix. Super assignment is used to access & set copy in the outer environment.
				inv <<- NULL			# Super assignment used to reset the inverse to null in the outer scope.
		}
		## get allows getting of the cached matrix
		get <- function() x				
		
		## setinverse allows setting of a new inverse which is stored in inv
		setinverse <- function(inverse) inv <<- inverse 
		
		## getinverse allows getting of the cached inverse in inv variable
		getinverse <- function() inv	

		## Returns the list of functions to get/set cached matrix and its inverse	
		list(set = set, get = get,		
			 setinverse = setinverse,
			 getinverse = getinverse)
}

##	cacheSolve(x,..)
##  	- x: input argument is the cached Matrix plus additional optional params.
##  	- Return value is the inverse of matrix. Retrieved from cache if already computed earlier.
cacheSolve <- function(x, ...) {
		## retrieving the stored (cached) inverse value.
		inv <- x$getinverse()
		
		## checking if an inverse is valid i.e. non null
		if(!is.null(inv)) {
				message("getting cached data")
				return(inv)  # using cached value
		}
		# cached inverse not available. getting the matrix 
		data <- x$get()
		
		# computing inverse
		inv <- solve(data, ...)
		
		# storing the newly computed inverse
		x$setinverse(inv)
		
		# returning the inverse
		inv
}
