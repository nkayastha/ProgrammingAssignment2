# makeCacheMatrix: This function creates a special "matrix" object that can cache its matinverse. 
# 1.set the value of the vector 
# 2.get the value of the vector 
# 3.set the value of the mean 
# 4.get the value of the mean 


makeCacheMatrix <- function(x = matrix()) { 
  matinv <- NULL 
  set <- function(y) { 
    x <<- y 
    matinv <<- NULL 
  } 
  get <- function() x 
  setmatinverse <- function(matinverse) matinv <<- matinverse 
  getmatinverse <- function() matinv 
  list(set = set, get = get, setmatinverse = setmatinverse, getmatinverse = getmatinverse) 
} 

## Write a short comment describing this function
## cacheSolve:  
## This function computes the matinverse of the special "matrix" returned by makeCacheMatrix above.  
## If the matinverse has already been calculated (and the matrix has not changed),  
## then the cacheSolve should retrieve the matinverse from the cache. 


cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the matinverse of 'x' 
  matinv <- x$getmatinverse() 
  if (!is.null(matinv)) { 
    message("Getting the Value for the required matrix") 
    return(matinv) 
  } 
  mat <- x$get() 
  matinv <- solve(mat, ...) 
  x$setmatinverse(matinv) 
  matinv 
} 
------------------------------------------------------------------------
#My Solution:
------------------------------------------------------------------------
#> my_matrix <- makeCacheMatrix(matrix(9:12, 2, 2))
#> my_matrix$get()
#     [,1] [,2]
#[1,]    9   11
#[2,]   10   12
#> cacheSolve(my_matrix)
#     [,1] [,2]
#[1,]   -6  5.5
#[2,]    5 -4.5
------------------------------------------------------------------------
