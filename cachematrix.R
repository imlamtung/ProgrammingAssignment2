##Create 4 objects for cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
                }
        get <- function()
                x
        setinverse <- function(inverse)
                i <<- inverse
        getinverse <- function()
                i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)        
}


##See if the matrix has been calculated. If yes, retrieve it from cache. If no, retrieve it through getinverse().

cacheSolve <- function(x, ...) {
i <- x
        $getinverse()
        if(!isnull(i)){
                return(i)
                }
data <- x
        $get()
        i <- solve(data,...)
        x
        $setinverse(i)
        i
}
