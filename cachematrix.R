# Matrix Cache function - input x is a square matrix
# w <- makeCacheMatrix(x) - this function creates a special "matrix" 
#      object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
# it first initialize m to be a NULL matrix
    m <- NULL                         # m is NULL
# it defines the set method and pass the argument to the input matrix
#    then nullify the inverse matrix
# w$set(e) - matrix e will be assigned to input x
# not used in this case
    set <- function(y) {              # define set method
        x <<- y
        m <<- NULL
    }
# w$get() will get the input matrix 
    get <- function() x
# set m = argument of w$setinv(m) 
# this is called from cacheSolve(w) to cache the m matrix
# define a method to save (cache) previous inverse
# set m equal to the passing argument
    setinv <- function(inverse) m <<- inverse
# define a method to get the previous inverse
# w$getinv() - retrieve the inverse matrix m if already exists
    getinv <- function() m
# this just lists the four method, not used in actual calculation
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

# x will be relaced by w from w <- makeInverse(x)
# this function is called by cacheSolve(w) - it computes the inverse
#     of the special "matrix" returned by makeCacheMatrix.  If the 
#     inverse has already been calculated (and the matgrix has not
#     changed), then the cacheSolve should retrieve the inverse from 
#     the cache
cacheSolve <- function(x, ...) {
    m <- x$getinv()                    # actually w$getinv()
    if(!is.null(m)) {                  # if m already exists
        message("getting cached data") # get the cached value
        return(m)                      # return value as m
    }
# if m has not been created
# set data equal to the input matrix retrieved from w$getinv()
    data <- x$get()                    # get the matrix, w$get()
    m <- solve(data, ...)              # use solve() to find inverse
    x$setinv(m)                        # set value m and cache
    m                                  # print the inverse
}
