## makeCacheMatrix() is used for defining a special "matrix" object
##   that stores the matrix itself and its inverse (default to be NULL).
## cacheSolve(mx) returns the inverse of a matrix by
##  - computing it if it has never been computed before;
##      it also caches the results for future calling of cacheSolve(mx)
##  - retrieving it from the cache if it has been computed before


## makeCacheMatrix() creates a special "matrix" object
##   that caches its inverse
## It also sets (or resets?) the cached inverse to NULL
## It contains a list of "sub-functions":
##   set(my): sets the matrix to be "my" (a user defined matrix), and
##            sets the cached inverse to NULL
##   get(): returns the matrix itself
##   setinv(mxinv): sets the cached inverse of the matrix 
##                   to a user-defined matrix "mxinv";
##   getinv(): returns the cached matrix's inverse
makeCacheMatrix <- function(mx = matrix()){
  mxinv <- NULL
  set <- function(my) {
    mx <<- my
    mxinv <<- NULL
  }
  get <- function() mx
  setinv <- function(definedinv) mxinv <<- definedinv
  getinv <- function() mxinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  
} 

## cacheSolve(mx) does the following:
##  If the inverse has been cached before 
##    (either by calling cacheSolve(mx) or mm$setinv(mxinv)),
##    cacheSolve(mx) returns the cached inverse directly 
##    (with a message reporting that it is "getting cached data")
##    without performing any inverse computation.
##  If the inverse has never been cached before (i.e., is NULL),
##    cacheSolve(mx) uses the solve() function in 
##    R to compute mx's inverse (mx is assumed to be invertible),
##    and updates the cached inverse in the special "matrix" object
##    to be the inverse obtained using solve() 

cacheSolve <- function (mx, ...){
  mxinv <- mx$getinv()
  if(!is.null(mxinv)) {
    message("getting cached data")
    return(mxinv)
  }
  data <- mx$get()
  mxinv <- solve(data, ...)
  mx$setinv(mxinv)
  mxinv
} 