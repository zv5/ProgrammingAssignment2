
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##modified by Son Nguyen Feb, 2021
makeCacheMatrix <- function(x =matrix()){
       m <- NULL
       
       ##use in case we need update the data in this matrix
       set <- function(y){
               x <<- y
               m <<- NULL
       }
       
       ##this function return value of input matrix
       get <- function() x
       
       ##this function save inverse matrix into cache data
       setinverse <- function(matrix) m <<- matrix
       #this function return the inverse matrix
       getinverse <- function() m
       
       list(set = set, get = get, setinverse = setinverse, 
            getinverse = getinverse)
}

cacheSolve <- function(x, ...){
        
       m <- x$getinverse()
        
       #if the inverse matrix already exist, the function will reuturn it
        if(!is.null(m)){
                message("getting cache data")
                return(m)
        }
        
      ##otherwise, we will calculate the inverse matrix the return it
        m <- solve(x$get())
        x$setinverse(m)
        m
     
}
