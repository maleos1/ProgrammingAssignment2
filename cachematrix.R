## makeCacheMatrix and cacheSolve are two functions which allow for the storing matrices, computing their inverses, 
## and caching the results. 

## makeCacheMatrix stores a list of functions (set, get, getinverse, setinverse) which do the following:
##   Using the "master" function, a matrix can be created, and, if desired, assigned to a variable (e.g. amatrix).
##   Using get() (e.g. amatrix$get() ), this matrix can be retrieved and printed
##   Using set() this matrix can be modified (e.g. amatrix$set(matrix(c(1,2,3,4))))
##   Using getinverse() (e.g. amatrix$getinverse() )the inverse of the matrix can be retrieved, if it has been previously computed and cached using cacheSolve;
##   otherwise the value printed will be NULL
##   setinverse() (e.g. amatrix$setinverse() ) allows to set a value for the inverse of the matrix; this function is called by 
##   cacheSolve in order to cache the inverted matrix that is computed by that function 
 
makeCacheMatrix <- function(x = matrix()) {
	  i <- NULL 
	  
	  set <- function(y) {
	  		x <<- y
	  		i <<- NULL
	  }
	  get <- function() x
	  
	  setinverse <- function(solve) i <<- solve  
	  
	  getinverse <- function() i
	  
	  list(set=set, get=get, getinverse=getinverse, setinverse=setinverse)

}


## cacheSolve takes a matrix created using makeCacheMatrix and caches its inverse.
## First it checks whether there is already a cached inverse of the matrix by calling getinverse() of makeCacheMatrix
##   If an inverse exists, it is returned 
##	 If an inverse does not exist, the function retrieves the original matrix by calling get() of makeCacheMatrix,
##	 computes the inverse, and, by calling setinverse() of makeCacheMatrix, caches the inverse as i (see above)

cacheSolve <- function(x, ...) {
                
        i <- x$getinverse()
        
        if(!is.null(i)) {
        	  message("getting cached data")
        	  return(i)
		}
		
		data <- x$get()
		
		i <- solve(data, ...)

		x$setinverse(i)
		i		  

}


