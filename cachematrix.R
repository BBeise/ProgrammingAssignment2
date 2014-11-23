# first function creates the special object that contains the functions to cache the inverse
  makeCacheMatrix <- function(x=matrix()) {
    mi <- NULL    
    #  mi is the 'inverse' and it's reset to NULL every time makeCacheMatrix is called
    
    #  note the next three functions will be used by cacheSolve() to get values for x or for
    #   mi (solve) and for setting the inverse.
    set <- function(y) {
      x <<- y
      mi <<- NULL
    }
    # get returns the value of the original vector and stores it using superassignment
    get <- function() { x }
    
    # set 'sets' the mi variable to the inverse
    setinverse <- function(solve)  { mi <<- solve }
    
    # getinverse returns the cached value to cacheSolve() on subsequent accesses 
    getinverse <- function() { mi }   
    
    # this is the list of methods that is the output object of makeCacheMatrix
    list(set=set, get = get, setinverse = setinverse, getinverse = getinverse)
  }
# second function computes or fetches the inverse
  cacheSolve <- function(x, ...) {
    # accesses the object 'x' created by makeCacheMatrix and gets the value of the inverse
    mi <- x$getinverse()
    # if inverse was already cached (not NULL) ...
    if(!is.null(mi)) {              
      # ... send this message to the console
      message("getting cached data")  
      # ... and return the inverse ... "return" ends the function cacheSolve()    
      return(mi)                       
    }
    # if x$getinverse() returned NULL (mi is NULL) then must calculate inverse
    data <- x$get()        
    mi <- solve(data, ...)
    # store the calculated inverse value in x (see setinverse() in makeCacheMatrix)
    x$setinverse(mi)           
    # and return the inverse to the code that called this function
    mi               
  }