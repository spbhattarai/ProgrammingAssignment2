
# making of the matrix than can cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      set <- function(Y){
        X <<- Y
        inverse <<- NULL
      }
      get <- function() X
      setinverse <- function(Inverse) inverse <<- Inverse
      getinverse <- function() inverse
      list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

#computes the inverse of the matrix returned by makeCacheMatrix. Also installs corpcor package if required

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  if(require("corpcor")){
    print("corpcor has been loaded correctly")
  } else {
    print("Installing corpcor")
    install.packages("corpcor")
    if(require(corpcor)){
      print("corpcor has been installed and loaded")
    } else {
      stop("could not install corpcor")
    }
  }
  inverse <- X$getinverse()
  if(!is.null(inverse)){
    message("matrix is in memory")
    return(inverse)
  }
  message("inverse is not in memory so the inverse (if exist) is being generated")
  data <- X$get()
  inverse <- pseudoinverse(data, ...)
  X$setinverse(inverse)
  inverse
}
