##Many Thanks to Lem Greski's Demystifying makeVector and Alan Berger's TIPS 
##makeCacheMatrix function creates and R object that stores a matrix and its inverse
##makeCacheMatrix intializes x as an empty matrix and assigns NULL to m in the parent environment



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Set assigns "y" to object x in the parent environment
  ## set assigns NULL to object m in the parent environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Due to lexical scoping get retrieves x from the parent environment of makeCacheMatrix
  get <- function() x
  ## Assign input argument to the value of m in the parent environment
  
  setinverse <- function(inverse) m <<- inverse
  ## Due to lexical scoping getinverse assigns correct symbol m to find inverse
  getinverse <- function() m
  ## return the list of functions to the parent environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is required to populate and or retrieve the inverse from an object of type makeCacheMatrix()

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## First it calls getinverse() function on the input object. Then it checks to see if it is NULL
  ## since makeCacheMatrix sets the inverse to NULL whenever a new matrix is set into the object,
  ## if the value here is not equal to NULL, we have a valid cached invers and 
  ## can return it to the parent envrionment
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If it is NULL then CacheSolve gets the matrix from the input object, calculates solve()and 
  ## uses setinverse() function on the input object to set the inverse in the input object, then returns
  ## the value of the inverse to the parent environment by printing the inverse object
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
