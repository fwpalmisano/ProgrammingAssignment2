# Goal of this assignment is to write a pair of functions called "makeCacheMatrix"
# and "cacheSolve" that cache the inverse of an invertible square matrix (ISM)

# "makeCacheMatrix" is a function that can create a special matrix object that can
# store the inverse of an ISM it receives as input in a "cache" 

# makeCacheMatrix receives an ISM as an input
makeCacheMatrix <- function(x = matrix()) {
        # instantitates a variable to eventually store its inverse
        inv <- NULL
        
        # creates a closure (function that stores data)
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        # "get" gets the value of the input matrix
        get <- function() x
        
        # "setInverse" solves for the value of the inverse of the original ISM
        setInverse <- function() inv <<- solve(x)
        
        # "getInverse" stores the value of the matrix found in "setInverse"
        getInverse <- function() inv
        
        # creates and returns the list that stores our special Matrix and it's inverse 
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

# "cacheSolve" receives one of the special matrix objects created by makeCacheMatrix
# and solves for the inverse of that matrix, but first checks to see if the 
# inverse matrix has already been calculated by checking the cache of the object it 
# receives as an input

cacheSolve <- function(x, ...) {
        # checks the getInverse argument (cache) for the input object
        inv <- x$getInverse()
        
        # if the cache is not empty, then it simply returns the stored value 
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        # if the cache is empty, then the function calculates the inverse of the ISM
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv      
}
