## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a special object that can store the inverse of a matrix
## cacheSolve returns the inverse of a matrix either from cache or by computing (if not available in cache)

## Write a short comment describing this function
## makeCacheMatrix creates a special object with 'get' and 'set' function
## to store the Matrix data as well as its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Assign NULL value to 'inv'
		inv <- NULL
		
        ## Defines 'set' as a function to set the variables 'inv' and 'x'
		set <- function(y) {
                x <<- y
                inv <<- NULL
        }
		
		## Defines 'get' as a function to return the matrix 'x'
        get <- function() x
		
		## Defines 'setinverse' as a function which assigns the inverse of the matrix to 'inv'
        setinverse <- function(inverse) inv <<- inverse
		
		## Defines 'getinverse' as a function which returns the inverse of the matrix as stored in 'inv'
        getinverse <- function() inv
        
		list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve tries to fetch the inverse of a matrix if it is available in the cache
## if it is not available, it computes the inverse of the matrix and returns the same
cacheSolve <- function(x, ...) {
		## Tries to fetch the inverse from cache
		i <- x$getinverse()
		
		## If 'i' is not null, it implies that the inverse value was available in cache
		## Correspondingly the inverse value is returned
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
		## If 'i' is null, it implies that the inverse value was not available in the cache
		## Next statement fetches the matrix using 'get' function
        mdata <- x$get()
		
		## Inverse of the matrix is calculated using 'solve' function, and assigned to 'i'
        i <- solve(mdata)
		
		## Inverse of the matrix, thus calculated above is stored in cache using 'setinverse' function
        x$setinverse(i)
		
		## Inverse of the matrix is finally returned back
        i
}
