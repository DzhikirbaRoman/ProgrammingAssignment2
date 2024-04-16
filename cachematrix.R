## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

###The following functions are used to create a special object that stores a matrix and caches its inverse. The first function, makeCacheMatrix creates a special “matrix”, which is really a list containing a function to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {   # set: Sets a new matrix y and resets the cached inverse (i) to NULL.
        x <<- y
        i <<- NULL
    }
    get <- function() x   # get: Returns the cached matrix x.
    setinverse <- function(inverse) i <<- inverse   #  setinverse: Sets the cached inverse of the matrix.
    getinverse <- function() i    # getinverse: Returns the cached inverse i.
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    # It returns a list containing these four functions, serving as an interface to interact 
    # with the cached matrix and its inverse.
}

## Write a short comment describing this function
# This function computes the inverse of the special “matrix” returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve 
# the inverse from the cache.
cacheSolve <- function(x, ...) { # This function is used to calculate the inverse of a matrix, with caching support.
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

matrix1 <- matrix(rnorm(25, 5, 1), nrow = 5, ncol = 5)
# matrix1
# View(matrix1)
matrix2 <- makeCacheMatrix(matrix1)
# matrix2
# data <- matrix2$get()
# data
# View(matrix2)
cacheSolve(matrix2)

cacheSolve(matrix2)


# makeVector <- function(x = numeric()) {
#     m <- NULL
#     set <- function(y) {
#         x <<- y
#         m <<- NULL
#     }
#     get <- function() x
#     setmean <- function(mean) m <<- mean
#     getmean <- function() m
#     list(set = set, get = get,
#          setmean = setmean,
#          getmean = getmean)
# }
# 
# cachemean <- function(x, ...) {
#     m <- x$getmean()
#     if(!is.null(m)) {
#         message("getting cached data")
#         return(m)
#     }
#     data <- x$get()
#     m <- mean(data, ...)
#     x$setmean(m)
#     m
# }
# 
# vec1 <- rnorm(10,1)
# vec1
# vec2 <- makeVector(vec1)
# vec2
# 
# cachemean(vec2)