## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(mat = matrix()) {
  
          inv <- NULL
       
          # set the Matrix   
          setmat <- function (matrix) {
            mat <<- matrix
            inv <<- NULL
          }
          
          # get the Matrix
          getmat <- function(){
            mat
          }
          
          # set inverse of Matrix 
          setInv <- function(inverse) {
            inv <<- inverse
          }
          
          # get inverse of Matrix
          getInv <- function(){
            inv
          }
          
          # list of results
          list(setmat = setmat, getmat = getmat, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(mat, ...) {
  
        ## Return a matrix that is the inverse of 'x'
        m <- mat$getInverse()
        
        # return the inverse if set
        if (!is.null(m)) {
          message ('get cached data')
          return(m)
        }
        
        # get the Matrix from obj
        data <- mat$get()
        
        # calculate the inverse
        m <- solve(data) %*% data()
        
        # set inverse to obj
        m$setInverse(mat)
        
        #return Matrix
        m
}
