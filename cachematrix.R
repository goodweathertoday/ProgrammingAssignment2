##this file contains are two functions makeCacheMatrix and cacheSolve 
##makeCacheMatrix returaned a list containg four functions that
##allows the user to set a matrix, get a matrix, set the inverse of the matrix
##and get the inverse of the matrix
##cacheSolve takes the list returned by makeCacheMatrix and calculates
##the inverse. First, cachesolve checks whether an inverse has already
## been cached. If not, it calculates it. 




#makeCacheMatrix returned a list of 4 items:
## 1. a funciton that sets the value of the matrix
## 2. a function that gets the value of the matrix
## 3. a function that sets the inverse of the matrix
## 4. a function taht gets the inverse of the matrix 


makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}

	get <- function() x
	
	setinverse <- function(inverse_arg) inverse <<- inverse_arg
	
	getinverse <- function() inverse

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  

}


##cacheSolve is a function that takes as its arguments
## the list returned by makeCacheMatrix
## the function does this by first checking whether
## a inverse is already cached. If not, it calculates it. 

cacheSolve <- function(x, ...) {
	inverse <-x$getinverse()
	
	if(!is.null(inverse)){
	message("getting cached data")
	return(inverse)
	}

	data <- x$get()
	inverse <- solve(data)
	inverse
	
}