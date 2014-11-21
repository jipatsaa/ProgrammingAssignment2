##makeCacheMatrix is a constructor function 
# atributes (or stored information):
#     x:        which stores the matrix itself
#     inv: which stores the inverse of the x matrix
# methods (or functions to apply over the stored information):
#     set(y):         assigns y value to the x attribute
#     get():          returns the value of the x attribute (the original matrix)
#     setinv(z): assigns z value to the inv attribute
#     getinv():  returns the value of the inv attribute
#quote: <<- is used to assign a value to a variable on a higher enviroment
###############################################################

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y      #change x attribute value by y
    inv <<- NULL # assign NULL to inv attribute
  }
  get <- function(){ 
         return(x)
  }
  
  setinv <- function(z) inv <<- z #change inv attribute  value by z
  getinv <- function() {
            return(inv)
  }
  

  list(set = set, get = get, # the list containing the methods to be return
       setinv = setinv,      # in order to make them visible from outside
       getinv = getinv)      # the object itself. 
  
}


## cacheSolve if the value of the inbersoa (inverse) attribute is null, 
##            then x$get obtains the original matrix 
##                 solve(data,...) obtains the inverse of the original
##                 x$setinbersoa(inbersoa) changes the value of the inbersoa attribute of x
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invAux <- x$getinv()
        if(!is.null(invAux)) {
          message("getting cached data")
          return(invAux)
        }
        data <- x$get()
        invAux <- solve(data, ...)
        x$setinv(invAux)
        return(invAux)
}
