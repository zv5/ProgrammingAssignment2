makeCacheMatrix <- function(x =matrix()){
       m <- NULL
       
       set <- function(y){
               x <<- y
               m <<- NULL
       }
       
       get <- function() x
       
       setinverse <- function(matrix) m <<- matrix
       getinverse <- function() m
       
       list(set = set, get = get, setinverse = setinverse, 
            getinverse = getinverse)
}

cacheSolve <- function(x, ...){
        m <- x$getinverse()
        
        if(!is.null(m)){
                message("getting cache data")
                return(m)
        }
        
        m <- solve(x$get())
        x$setinverse(m)
        m
     
}