# |*****************************************************************************
# | Dwayne Macadangdang 8/20/2016
# | Week 3 Programming Assignment #2
# | Forked from https://github.com/rdpeng/ProgrammingAssignment2 
# |*****************************************************************************

# | dependencies
# | setwd("/Users/mistermaxx/Documents/work/personal/Coursera/R_Programming/Week_3/ProgrammingAssignment2")

# |*****************************************************************************
# | Returns a list containing four functions that:
# | 1. Sets the value of the matrix
# | 2. Gets the value of the matrix
# | 3. Sets the value of inverse of the matrix
# | 4. Gets the value of inverse of the matrix
# | Receives a matrix "x" as an argument
# | (This reminds me very much of creating a class containing member variables,
# | public methods, and private get/set methods in C#)
# |*****************************************************************************
makeCacheMatrix <- function(x = matrix())
  {
    # initiate the default value of the matrix inverse
    inv <- NULL
    
    # create function to set the value of the matrix
    set <- function(y)
      {
        x <<- y
        inv <<- NULL
      }
    
    # create the function to get the value of the matrix
    get <- function() {x}
    
    # create the function that sets the value of the inverse of the matrix
    setinverse <- function(inverse) {inv <<- inverse}
    
    # create the function that gets the value of inverse of the matrix
    getinverse <- function() {inv}
    
    # return the list of four functions
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)    
  }

# |*****************************************************************************
# | This function computes the inverse of the special "matrix" returned by
# | makeCacheMatrix above
# |*****************************************************************************
cacheSolve <- function(x, ...)
  {
    # check cache: inv not null?
    if(!is.null(x$getinverse()))
      {
        message("Data retrieved from the cache.")
        inv <- x$getinverse()
      }
    else
      {
        # use solve() to process the data
        message("Data processed using solve(), then cached.")
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
      }
    return(inv)
  }
