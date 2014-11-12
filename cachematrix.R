#Matrix inversion is usually a costly computation and there may be some benefit to caching 
#the inverse of a matrix rather than computing it repeatedly 

#The first function, makeCacheMatrix 
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse
#4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) inv <<- solve
  getmatrix <- function() inv
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


#The following function calculates the inverse of the output from the above function. 
#It first checks to see if the inverse  has already been calculated. If so, it gets the 
#inverse from the cache and skips the computation. Otherwise, it calculates the inverse
#of the data and sets the value of the inverse in the cache via the setmatrix function.

cacheSolve <- function(x, ...) {
  inv <- x$getmatrix()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setmatrix(inv)
  inv
}

#Test the functions
x=rbind(c(1,3),c(2,4))
amatrix = makeCacheMatrix(x)
amatrix$get() 

cacheSolve(amatrix)
amatrix$getmatrix() 

cacheSolve(amatrix) 
y=rbind(c(0,99),c(5,66))
amatrix$set(y) 

cacheSolve(amatrix) 

amatrix$get() 
amatrix$getmatrix() 

