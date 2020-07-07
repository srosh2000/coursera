#makeCacheMatrix function creates a special "matrix" object which enables us to cache the inverse of the matrix
makeCacheMatrix <- function(x = matrix()){ #defining the argument 
  inv<- NULL #assigning NULL to the inv variable which will hold the value of the inverse matrix 
  set<- function(y){ #setting the value of the matrix 
    x<<-y #assigning the value of matrix in parent environment 
    inv<<- NULL #inv reset to NULL in case of new matrix 
  }
  get<- function() {x} #defining "get" function which returns the value of the matrix argument 
  setInverse<- function(inverse) {inv <<- inverse} #assigns value of inv in parent environment 
  getInverse<- function() {inv} 
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

#cacheSolve function computes the inverse of the special "matrix" created by the makeCacheMatrix above              
cacheSolve<- function(x, ...){
  inv<- x$getInverse() 
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
    
  }
  mat<- x$get()
  inv<- solve(mat, ...)
  x$setInverse(inv)
  inv
}
