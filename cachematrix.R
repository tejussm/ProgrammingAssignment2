## The functions below compute and cache the inverse of a matrix. Matrix inversing is a costly computation that
## is best performed once and the results cached for later reuse (to save cost/time repeating the inversing)

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {                                #sets the value of the matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x                                 #gets the value of the matrix
    setinverse <- function(inverse) inv <<- inverse     #sets the value of the inverse
    getinverse <- function() inv                        #gets the value of the inverse
    list(set = set, get = get,                          #create a named list forlater reference/function calls
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function computes the inverse of the special matrix returned by makeCacheMatrix above. However, if the 
## inverse has already been calculated and matrix has not changed, then it retrieves the inverse from the cache
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()                               #gets the inverse from previous function
    if(!is.null(inv)) {                                 #if inverse exists (not NA), return it and exit function
        message("getting cached data")
        return(inv)
    }
    data <- x$get()                                     #otherwise get the matrix from above
    inv <- solve(data, ...)                             #and create its inverse with solve function
    x$setinverse(inv)                                   #store it into cache with function above for later use
    inv   ## Return a matrix that is the inverse of 'x' #return the inverse and exit this function
}
