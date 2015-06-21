####
####      The functions below are used to create a special object that
####      stores a matrix (of numeric values) and then cache that matrix's 
####      inverse



####      The makeCahceMatrix function creates a special "matrix" assuming it is
####      passed a square and invertible matrix x. It returns a list of functions 
####      (setting the matrix, getting the matrix, setting the inverse, and getting 
####      the inverse). This list of functios is then passed on to the function cacheSolve
####      which is defined further below.

####      I follow the sript in makeVector and cachemean very closely

makeCacheMatrix <- function(x = matrix()) {
          
          invr <- NULL
          
          ## Define the set and get functions
          set <- function(y){
              x <<- y
              invr <<- NULL
              }
          get <-function() x
          ## Then the setinverse and getinverse
          setinverse <- function(inverse) invr <<- inverse
          getinverse <- function() invr
          
          ## Finally the list of functions this function gives
          list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  }




####      The cacheSolve function takes as argument the "x" or the matrix produced
####      by the makeCacheMatrix function and it returns the inverse of that matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          
          invr <- x$getinverse()
          ##    if the stored value is not NULL and the inverse has been calculated 
          ##    or given then take it and skip calculations using "solve" 
          if (!is.null(invr)){
              message("getting cached data")
              return(invr)
              }
          ##    else calculate the inverse of x
          matrix.data<- x$get()
          invr <- solve(matrix.data, ...)
          x$setinverse(invr)
          
          invr
  }
