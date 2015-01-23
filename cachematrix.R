
#This first function will input a matrix and save it globally. If it is the first time this
#matrix is used in this function the inv parts will be blank since it has not been saved
#from the cacheinv function

makematrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y #The double arrow sets is to the global
    m <<- NULL 
  }
  #These next steps set the 4 parts of the makematrix output.
  
  get <- function() x
  setinv <- function(solve) m <<- solve #The first time this is output the setinv, getinv, 
  #will be null since m is null
  
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#This function is used to calculate the inverse of a matrix made in the make matrix function.
#The first time a matrix is run through this function, it will calculate the inverse.
#If after the first time, then the inverse has already been stored and it will be retrieved from
#the global environment. 

cacheinv <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) { #if the inverse has already been calculated it will retrieve the inverse 
    #from this section, from the global environment
    message("getting cached data")
    return(m)
  }
  #if the inverse has not been calculated, the data will go through this section of the loop
  #and then stored in the global environment (m)
  #We will then be able to use the makematrix function and see the calculated inverse
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}