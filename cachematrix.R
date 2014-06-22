# Here I will present two functions. The first one, 'makeCacheMatrix()', creates a special object
# (class: "matrix") that can cache its inverse. While the second function 'cacheSolve()' will 
# compute the inverse of the matrix returned from 'makeCacheMatrix()'. If the inverse was already
# calculated the function shouldn't perform any new calculations but instead return the previously
# cached value for the inverse.

# First Function: 'makeCacheMatrix()'
makeCacheMatrix <- function(m4trix = matrix()){      	# Here m4trix is the input variable and it
							# has to be a matrix. Note that the matrix 
							# must not be singular so that an inverse 
						        # can exist. If it is not 'cacheSolve()' 
							# will point it out.
	inverse <- NULL					# The first time a vector is defined there 
						        # is no computed inverse thus I define a 
						        # local variable to be an empty matrix.
	set <- function(y){				# Here I define the global values for m4trix 
							# and inverse.
		m4trix <<- y
		inverse <<- NULL
	}
	get <- function() m4trix		        # This just defines the no input function 
							# 'get()' that returns m4trix on call
	setinverse <- function(solve) inverse <<- solve	# Here I define the 'setinverse()' function
							# that globally gives inverse the value of 
							# solve. Note: here solve is just a place 
						        # holder, it has little to do with the 
							# function 'solve()'; I just use it to 
						        # remind me of what it is used for. This is
						        # manipulated by the 'cacheSolve()' function
							# that will pass this information to the 
							# function once it has processed a "special 
							# matrix". A special matrix is one that has 
							# processed by 'makeCacheMatrix()'
	getinverse <- function() inverse		# Here the no input function 'getinverse()'
							# is defined. It just returns the value of 
							# inverse. If the matrix has never been 
							# processed by 'cacheSolve()' before this
						        # is empty.
	list(set = set, get = get,	                # This returns a list of the cached info 
	setinverse = setinverse,		        # that the 'makeCacheMatrix()' returns. This
	getinverse = getinverse)			# info can be later accessed and modified by 
							# other functions. 

}

# Second Function: 'cacheSolve()'
cacheSolve <- function(m4trix, ...){			# Here I begin defining the Second main 
							# function, 'cacheSolve()', that takes the
							# a argument, 'm4trix' but also inherits the
							# arguments of its child functions. Note: I 
							# assume that the m4trix argument here is a
							# matrix that has been processed through 
							# 'makeCacheMatrix()'.
	inverse <- m4trix$getinverse()			# Here I extract the inverse from the cached
							# matrix 
	if (!is.null(inverse)){				# I check if the inverse has already been 
		message("getting cached data")		# computed. If the memory that stores the
		return(inverse)				# inverse value is not empty, e.g. if the
							# inverse of the m4trix has already been 
							# computed (thus saved in cache memory), 
							# return the inverse value stored in memory 
							# instead of computing the inverse anew.
	}						# Note: the 'return()' function ends the
							# function. Thus if the m4trix argument has
							# been previously passed the function will
							# return the previously saved inverse and 
							# then stop.
	data <- m4trix$get() 				# If, however, the inverse hasn't been 
							# calculated before we access the initial 
							# form of the "special matrix" and then
	inverse <- solve(data, ...)			# calculate the inverse using 'solve()'
	m4trix$setinverse(inverse)			# We then save the value of the inverse 
							# attribute for the already processed 
							# "special matrix" and cache it in the 
							# working memory.
	inverse				        	# Finally we return the inverse.
}
# The output should always return the inverse of the matrix that has been processed by 
# 'makeCacheMatrix()'. In case the "special matrix" has been previously processed then it will 
# remember it from the cache and the output should inform you by saying "getting cached data"
