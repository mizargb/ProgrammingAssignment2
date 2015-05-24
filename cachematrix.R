## As for the cache vector we create a function tu create special matrices with
## the value i to cache computed inverse and the flag c to detect changes
## the cacheSolve will then see if there is a matrix cached in "i", check if there
## have been changes after i was computed and either return the cached inverse or 
## re-compute. I know probably c flag is not needed but i wanted to explicitely
## show the check on changes instead of just relying on i being equals to null

## Just like in the examplke given on vector cachedmean we create an object 
## list with matrix and cached values, get/set methods for them and set up default values 
## on creation such that when we call this function with a matrix passed as argument
## we get as result a list with the base matrix(getMatrix) and inverse matrix
## (i--> null)
## a flag set on TRUE for change detection (c) and "set" which is a function to 
## initialize the object "specialmatrix" with base matrix set as the argument
## passed and default inverse(null) and change flag(true)

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  c <- TRUE
  set <- function(y) {
    x <<- y
    i <<- NULL
    c <<- FALSE
  }
  getMatrix <- function() x
  setInverse <- function(inv) i <<- inv
  getInverse <- function() i
  setChange <- function(ch) c <<- ch
  getChange <- function() c
  list(set = set, getMatrix = getMatrix,setInverse = setInverse,
       getInverse = getInverse, setChange=setChange, getChange=getChange)

}


## IF we already computer the inverse(x$getInverse is not null) and there were 
## not changes on the matrix after that computation, we return the matrix, else we
## compute it, return it and set the flag "Changed" to false because the inverse is
## again up to date

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
    i <- x$getInverse()
    c <- x$getChange()
    if(!is.null(i)&c) {
      message("getting cached data")
      return(i)
    }
    data <- x$getMatrix()
    i <- solve(data, ...)
    x$setChange(TRUE)
    x$setInverse(i)
    i
}
