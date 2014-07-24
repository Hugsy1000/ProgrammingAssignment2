#Two functions that cache the inverse of a matrix


#Creates matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

#Initialise the inverse property
    i <- NULL

    #To set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    #To get the matrix
    get <- function() {
    #To return the matrix
     m
    }

    #To set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    #To get the inverse of the matrix
    getInverse <- function() {
        #To return the inverse property
        i
    }

    #To return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


#Compute the inverse of the special matrix returned by "makeCacheMatrix"
#above. If the inverse has already been calculated (and the matrix has not
#changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    #To return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    #Only return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    #Get the matrix from object
    data <- x$get()

    #Calculate the inverse
    m <- solve(data) %*% data

    ## Set the inverse to the object
    x$setInverse(m)

    #Return matrix
    m
}
