#Coursera Data Science Specialisation. R Programming Assignment 2.
#These two functions together show how to create a matrix objet that can cache its inverse, 
#The functions allow the matrix to be displayed, and to get, set and cache the inverse


#create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      #return the matrix
      get <- function() x
      #get set the inverse
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      #list of available functions
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

#check to see if the inverse has already been calculated.
#if it has - return it
#if not calculate the inverse and cache the value
cacheSolve <- function(x, ...) {
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      #get the matrix
      data <- x$get()
      #calculate the inverse
      m <- solve(data, ...)
      #cache the value
      x$setinverse(m)
      m
}