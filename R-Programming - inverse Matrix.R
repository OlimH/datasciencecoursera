library(matlib)

#This function will create a special matrix, the input for this cache function is of type matrix.

makeCacheMatrix <- function(x = matrix()){
  
  m=NULL
  
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m

  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

#This function will return the inverse of the matrix, I use the library matlib to get the inverse.

cacheinv <- function(x, ...) {
  m <- x$getinv()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  f <- x$get()
  
  m <- inv(f, ...)
 
  x$setinv(m)
  
  m
}

#ran these tests
x <- matrix(c(5,8,7,10,4,-5,-2,4,6),3,3)
d <- makeCacheMatrix(x)
cacheinv(d)

#output
#> x <- matrix(c(5,8,7,10,4,-5,-2,4,6),3,3)
#> d <- makeCacheMatrix(x)
3> cacheinv(d)
#           [,1]       [,2]       [,3]
#[1,]  0.2820513 -0.3205128  0.3076923
#[2,] -0.1282051  0.2820513 -0.2307692
#[3,] -0.4358974  0.6089744 -0.3846154
#>
#

