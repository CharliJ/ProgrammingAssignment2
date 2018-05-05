## The functions 'makeCacheMatrix' and 'cacheSolve' work in tandem to calculate and
# cache/store the inverse of a matrix.  The functions will check and see if there 
# is already a cached calculation before re-calculating.  This makes it so if the
# inverse matrix is needed again later, it doesn't have to be re-calculated again,
# which can be time-consuming.  Use them like this:
# a<- 'your matrix to be inverted'
# b<- makeCacheMatrix(a)
# cacheSolve(b)  (this does the original calculation)
# cacheSolve(b)  (running it a second time on the same 'makeCacheMatrix' output
# will retrieve the inverted matrix from cache) 


## The 'makeCacheMatrix' accepts the matrix to be inverted as an input, and outputs
#a list that's in the format required by 'cacheSolve'.  This makes it so cacheSolve
#can check if there already is an inverted matrix calcaulted for the input matrix.
#If so, it outputs it.  If not, it calculates a new one and stores it.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvert <- function(invert) m <<- invert
        getinvert <- function() m
        list(set = set, get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


## 'cacheSolve' accepts an input in the format outputted by 'makeCacheMatrix'. 
#'cacheSolve' checks to see if there is already an inverted matrix stored.  If
#there is, it returns it.  If not, it calculates one, stores the result in the list
#created by 'makeCacheMatrix', and prints the inverted matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvert()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvert(m)
        m
}
