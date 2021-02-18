## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
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

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getInverse()
        
        # return the inverse if set
        if (!is.null(mat)) {
          message ('get cached data')
          return(mat)
        }
        
        # get the Matrix from obj
        data <- x$get()
        
        # calculate the inverse
        mat <- solve(data) %*% data()
        
        # set inverse to obj
        x$setInverse(mat)
        
        #return Matrix
        mat
}
