## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  
  inv  <-NULL
  set<-function(y){
    x   <<- y
    inv <<- NULL
  }
  get <- function()x
  setInv <-function(inverse)inv<<-inverse
  getInv  <- function()inv
 
   list(set=set,get=get,setInv=setInv,getInv=getInv)
  
  }
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inv = x$getInv()
 
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <-x$get()
  inv  <-solve(data,...)
  x$setInv(inv)
  inv
}

data = matrix(data = 1:4,2,2)
print(data)
one = makeCacheMatrix(data)
two = cacheSolve(one)
print(two)
three  = cacheSolve(one)
print(three)


