## Assignment 2 for Coursera R Programming Class
## Author: Charles Corbalis
## 05/23/2015
##
## This file consists of two functions that are used to calculate
## the inverse of an invertible matrix. Since such a computation is 
## expensive, these functions allow the results of an inversion to be
## cached and returned if the inverse has already been calculated.  
## 
## Usage:
## To create a "cacheable" matrix run the function makeCacheMatrix then use the $setCM 
## function for this object to set its value. For example if A is a matrix:  
## 			B<-makeCacheMatrix(A)   
## 			B$setCM(A)
## will create a cacheable matrix version of A named B.
## 
## Now to get the inverse of this matrix execute:
## 			cacheSolve(B)
##
## if the inverse has already been calculated, it will return the  cached inverse, 
## otherwise it will calculate and return the inverse, saving the result for the future.
##
## Limitations:
## These functions assume that the matrices used are invertible. They do not check for 
## invertbility; hence an error will be generated if they are not.
##
#########################################################################################


## This function creates an cached matrix object and creates a set of five functional
## operators for the object that are used to manipulate it.

makeCacheMatrix <- function(x = matrix()) {
	flag <- FALSE
	
	#This functional operator sets the value of the matrix. In addition, it creates a duplicate
	#as a placeholder for its inverse and a flag that indicates if the inverse has been calculated.
	
		setCM<-function(y) {
			flag <<- FALSE
			x <<- y
			x.inv <<- x
		}
	
	#This functional operator returns the value of the cached matrix object.
	
		getCM <- function() x
	
	#This functional operator sets the value of the inverted matrix and set
	#the flag to true indicating that an inverse has been cached.
	
		setInv <- function(w) { 
			inv.x <<- w
			flag <<- TRUE
		}
	
	#This functional operator returns the previously cached inverse
	
		getInv <- function() inv.x
	
	# This functional operator determines if an inverse has been cached
	# returning either true or false.
	
		getInvExists <- function() flag
	
	list( setCM = setCM,
			getCM = getCM,
			getInv = getInv, 
			setInv = setInv,
			getInvExists = getInvExists)
}


## This function returns the inverse of a cacheable matrix object.
## If the inverse exists already, it simply fetches it and returns it
## Otherwise, it calculates the inverse and caches it for future requests. 

cacheSolve <- function(x, ...) {

	flag <- x$getInvExists()  # check to see if inverse exists already
	if (flag) {
		message("getting from cached inverses")
		return(x$getInv())   # If so, the return the cached value
	}
	# Otherwise, calculate the inverse, cache it and return it.
	dat <- x$getCM()
	dat.inv <- solve(dat, ...)
	x$setInv(dat.inv)
	dat.inv	
}
