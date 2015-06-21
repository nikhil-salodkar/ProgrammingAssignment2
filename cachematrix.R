#makeCacheMatrix and cacheSolve are two functions that work together to create matrix objects and store its
#inverse for later use. First makeCacheMatrix function is to be used to create an object containing a matrix,
#and then cashSolve can be used on the object created from makeCacheMatrix to compute or get the inverse of
#matrix contained in object.

#makeCacheMatrix function is a list of functions that takes a matrix as argument and stores it in an object.
#Later, functions can be called on the object such as getting matrix from object using get function, setting 
#matrix of the object using set function, getting and setting inverse of the matrix contained in object using 
#getInverse and setInverse functions.

makeCacheMatrix <- function(x = matrix()) {
  #inv stores the inverse of the input_matrix  
  inv <- NULL
  #initially inv value will be NULL
  set <- function(matrix2){
    x <<- matrix2
    inv <<- NULL
    #whenever a new matrix is set in object inv can change thus it is made NULL.
  }
  get <- function(){
    x
  }
  getInverse <- function(){
    inv
  }
  setInverse <- function(inv_matrix){
    inv <<- inv_matrix
  }
  list(set = set, get = get, getInverse = getInverse, setInverse = setInverse)
}


#The cacheSolve function is used to compute inverse of a matrix contained within an object generated using 
#createCacheMatrix function. It takes initially created object as argument and returns the inverse of the matrix
#contained in the object. If the object has already computed the inverse then inverse is not computed again but
#instead value of inverse is retrieved from the object itself. If the object does not contain precomputed
#inverse value then inverse is calculated for the matrix present in the object and the inverse is stored in the
#object for later use.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  #This code below checks whether inverse is already computed. If yes then
  #existing value is returned.
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  #if inverse is not present, then get matrix from object and compute inverse using solve function.
  data <- x$get()
  inv  <- solve(data)
  #now store the inverse value back in the object for later use.
  x$setInverse(inv)
  inv
}
