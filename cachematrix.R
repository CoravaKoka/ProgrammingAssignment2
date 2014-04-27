
# The program cashematrix.R consists of two functions that are used to create a special object 
# that stores a numeric matrix and caches its matrix inverse.
 
# If the contents of a matrix are not changing, it may make sense to cache the value of the 
# inverse matrix so that when we need it again, it can be looked up in the cache rather than recomputed.


# Function makeCacheMatrix
# This function creates a special "matrix" object that can cache its inverse.
# Function makeCacheMatrix takes as an argument matrix x, and returns a list with four 'objects' - functions
# Main purpose of this function is to store and display matrices - x and  im (x-inverted)

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL                         ## assigning NULL to im
  
  set <- function(y) {               ## defines a function set
    x <<- y                          ## assigning y to x
    im <<- NULL                      ## assigning NULL to im
  }
  
  get <- function() x
  setInverse <- function(solve) im <<- solve           ## defines a function setInverse with argument solve, and pass argument solve to the im
  getInverse <- function() im                          ## defines a function getInverse
  list(set = set, get = get,                           ## return from the function makeCacheMatrix - list with four functions
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
# should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  im <- x$getInverse()
  
  if(!is.null(im)) {                            ## checking does im is NULL or not, and if im is not NULL value
    message("getting cached data")              ## printing message
    return(im)                                  ## return inverted matrix
  }
  
  data <- x$get()                               ## assigning data with matrix x
  im <- solve(data, ...)                        ## calculate inverse matrix
  x$setInverse(im)
  im
}



##     TESTING     ##
#####################


x <- matrix(1:4,2)
a <- makeCacheMatrix(x)
a$get()
a$getInverse()
cacheSolve(a)
cacheSolve(a)
a$getInverse()
b <- a$getInverse()
b %*% x


x <- matrix(c(1, 5, 2, 3, 1, 8, 2, 7, 3),3)
a$set(x)
a$get()
cacheSolve(a)
cacheSolve(a)
a$getInverse()
b <- a$getInverse()
b %*% x


x <- matrix(rnorm(100), 10, 10)
a$set(x)
a$get()
cacheSolve(a)
cacheSolve(a)
a$getInverse()
b <- a$getInverse()
b %*% x




