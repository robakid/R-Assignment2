##assignment 2: Robakidze, Ana 
## below are functions that cache and compute the inverse matric 
   makeCacheMatrix <- function(x = matrix()) {
      
          xinv <- NULL # this is where the result of inversion is stored
           # A setter function, use this to set a matrix to object created by makeCacheMatrix function
         
           set <- function(y) {
                   x <<- y
                   xinv <<- NULL # it also initialises xinv to null
               }
             
             get <- function() x # return the input matrix
               setInv <- function(inv) xinv <<- inv # set the inversed matrix
               getInv <- function() xinv # return the inversed matrix
            
                list(set = set, get = get,
                               setInv = setInv,
                                getInv = getInv)
           }
   
    
     cacheSolve <- function(x, ...) {
           m <- x$getInv() # get the inversed matrix from object x
       # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
         if(!is.null(m)) { # if the inversion result is there
              message("getting cached data")
              return(m) # return the calculated inversion
      }
       data <- x$get() 
       m <- solve(data) 
         x$setInv(m) 
          m # return the solved result
}
     # time for testing
       test <- matrix(runif(9,1,100),3,3)
      testCached <- makeCacheMatrix(test)
      
