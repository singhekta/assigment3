#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute 
#it repeatedly
#this  assignment includes e a pair of functions that cache the inverse of a matrix.
#creates a special “matrix” object that can cache its inverse.
#The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  #  set the value of the matrix
  set <- function(y) {
          x <<- y
          inv <<- NULL
  }
  
  get <- function() x  #  get the value of the matrix
  
  setinverse <- function(inverse) inv <<- inverse #  set the value of inverse of the matrix
  
  getinverse <- function() inv  # get the value of inverse of the matrix
 
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# function to compute the inverse of "matrix"

cacheSolve <- function(x, ...) {
  
   inv <- x$getinverse()
   
    if(!is.null(inv)) {
           message("getting cached data")
           return(inv)
  } # checks if the inverse has already been computed,
    # if yes, it gets result
   # else it computes the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv) # set the value in the cache via setinverse function.
  inv
}

#Running the code

 # > source('C:/Users/estjo/Desktop/coursera/assg_3/programming assignment 2_Lexical Soping.R')

#> matrix_1 <- makeCacheMatrix(matrix(1:4, 2, 2))
#> matrix_1$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4

#> matrix_1$getinverse()
#NULL
# no cache in first run
#> cacheSolve(matrix_1)
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
# retrieving from the cache in the second run
#> cacheSolve(matrix_1)
#getting cached data
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#> matrix_1$getinverse()
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

#> matrix_1$set(matrix(c(2, 2, 1, 4), 2, 2))
#> matrix_1$get()
#[,1] [,2]
#[1,]    2    1
#[2,]    2    4

#> matrix_1$getinverse()
#NULL

#> cacheSolve(matrix_1)
#[,1]       [,2]
#[1,]  0.6666667 -0.1666667
#[2,] -0.3333333  0.3333333

#> cacheSolve(matrix_1)
#getting cached data
#[,1]       [,2]
#[1,]  0.6666667 -0.1666667
#[2,] -0.3333333  0.3333333

#> matrix_1$getinverse()
#[,1]       [,2]
#[1,]  0.6666667 -0.1666667
#[2,] -0.3333333  0.3333333
