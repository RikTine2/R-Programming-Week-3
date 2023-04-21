# The makeCacheMatrix and cacheSolve functions can be used when a matrix 
# is repeatedly inverted in a loop. The functions perform the inversion
# and store the result in cache memory so that the expensive inversion
# routine does not have to be repeated

# The makeCacheMatrix function produces a vector of functions that
# allows the matrix to be set in memory ($set), retrieves the matrix from
# memory ($get), places the matrix inverse into memory ($setinverse), and 
# recalls the inverse from memory ($getinverse).

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function() {                 # Lines 13-15 create the $set() function
                                        # that is used to put the matrix in memory
        x <<- z
        m <<- NULL
    }
    get <- function () {x}              # This line establishes the $get() function
                                        # that allows the matrix to be pulled from memory
    setinverse <- function(x) {         # Lines 20-22 establish the $setinverse() function
                                        # that allows the inverse to be placed in memory
        m <<- solve(x)
        }
    getinverse <- function() {m}        # This line establishes the $getinverse() function
                                        # that allows the inverse to be pulled from memory
    list (set = set, setinverse = setinverse, 
        get = get, getinverse = getinverse)     # These lines create the list of functions
                                                # that are used in cacheSolve()}
}


# The cacheSolve function calls makeCacheMatrix to set up memory storage
# for the matrix and its inverse, performs the inverse operation, and 
# calls the inverse from memory if the inverse has already been calculated.
# To perform the matrix inversion cache operation use command cacheSolve(X)
# where X is a square nonsingular matrix.

cacheSolve <- function(x, ...) {
    setFunc <<- makeCacheMatrix(z)    # This line sets up cache memory
    m <- setFunc$getinverse()         # Lines 35-39 determine whether the inverse
                                      # has already been calculated and returns
                                      # the inverse from memory if it has
    if(!is.null(m)) {
        print("cached inverse...")
        return(m) 
    } else {                          # Lines 41-46 calculate the inverse and
                                      # place it in memory if the inverse has
                                      # not already been calculated
        data <- setFunc$get()
        m <- solve(data) 
        setFunc$setinverse(m) 
        print ("newly calculated inverse...")
        m
    }
}
