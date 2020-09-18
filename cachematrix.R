
#matrix is supposed to be a invertible matrix

makeCacheMatrix <- function( x )
{
    m <- NULL
    set <- function(y)
    {
        x <<- y
        m <<- NULL        
    }
    get <- function() x
    setInvMatrix <- function(solve) m <<- solve
    getInvMatrix <- function() m
        
    #here we make the list of the functions which we are going to need for cache 
    
    list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)

}


# from here we can check the matrix inverse is cached or not 
cacheSolve <- function( x , ...)
{
    m <- x$getInvMatrix()
        
    #this function checks if m is null or not
    
    if(!is.null(m))
    {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInvMatrix(m)
    m

}
