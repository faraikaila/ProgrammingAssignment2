## make cache matrix creates a "vector" to cache calculated inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y){
                x <<-y
                inv <<-NULL
        }
        
        get<- function() inv
        setInv <- function(inverse) inv<<-inverse
        getInv <- function()inv
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## cacheSolve checks if inverse has been evaluated before, if not calculates it, if it has been, fetch if from the cache and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getInv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        data<-x$get()
        inv <- solve(data,...)
        x$setInv(inv)
        inv
}
