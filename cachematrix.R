
#matrix is supposed to be a invertible matrix


makeCacheMatrix <- function( x = matrix() )
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

    list(set = set, get = get, setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)

}

cacheSolve <- function( x , ...)
{
    m <- x$getInvMatrix()
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


