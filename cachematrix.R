makeCacheMatrix <- function(x = matrix()) 
{
  
  xinverse <- NULL 
  set <- function(y) 
  {
    x <<- y
    xinv <<- NULL 
  }
  get <- function() x 
  setinv <- function(inv) xinverse <<- inv 
  getinv <- function() xinverse 
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


cacheSolve <- function(x, ...) 
{
  m <- x$getinv() 
  if(!is.null(m)) 
  { 
    message("getting cached data")
    return(m) 
  }
  ans <- x$get() 
  m <- solve(ans) 
  x$setinv(m) 
  m 
}