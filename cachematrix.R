## This assignment is about solving the inverse of a matrix by caching the 
# result within a lexical scope of a function:  "makeCacheMatrix" and 
# "cacheSolve". Caching is about using memory to avoid excess computation.
# Lexical scopes, allow to create functions within a function and new 
# "user defined" objects (data types) to store data within several environments
###################################

###################################
## The function "makeCacheMatrix" creates a new, unique environment. 
# The inverse matrix is cached inside the object m, within the main 
# environment, which is unique for EACH instance the function is called.
## The output of the function is a list with 5 named elements, which are 
# the five functions defined herein: setmatrix, getmatrix, setinverse, 
# getinverse and getenv
###################################
###################################

makeCacheMatrix <- function(x = matrix()) {
  

  m<-NULL
  evn <- environment()  
  y<-NULL 
  
  setmatrix<-function(y){  
    x<<-y  
    m<<-NULL 
  }
  
  getmatrix<-function() x  
  setinverse<-function(solve) m<<- solve  
  getinverse<-function() m  # Get the saved value of inverse matrix m that was saved with setinverse
  getenv<- function() environment()
  
  list (setmatrix=setmatrix, getmatrix = getmatrix, # creates list to house the four functions  
        setinverse = setinverse,
        getinverse = getinverse,
        getenv = getenv)
  
}

###################################
## The function "cacheSolve" returns the inverse of the matrix that is 
# returned by makeCacheMatrix function, e.g. xMat$getmatrix()
###################################

cacheSolve <- function(xMat= m(), ...) {
 
  m <- xMat$getinverse() 
  if(!is.null(m)){ 
    if(xMat$setmatrix() == xMat$getmatrix()) {
      message("getting cached data")
      matrix<-xMat$get()
      m<-solve(matrix, ...)
      xMat$setmatrix(m)
      return(m) 
    }
    # Else
    y <- xMat$getmatrix()
    xMat$setmatrix(y) 
    m <- solve(y, ...)
    xMat$setinverse(m) 
    m 
  }
 
}