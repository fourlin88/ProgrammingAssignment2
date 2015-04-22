## This R program is a part of assignment 2
## makeCacheMatrix is a function that creates and initializes the cache. 
## This fucnction requires one parameter x with object type as matrix. 
## 

makeCacheMatrix <- function(x=matrix()) {
tmpinv <- NULL
set <- function(y) {
   x <<- y
   tmpinv <<- NULL
}
get <- function() x
setinv <- function(solve) tmpinv <<- solve
getinv <- function() tmpinv
list(set = set, get = get,
setinv = setinv,
getinv = getinv)
}

##
## cacheSolve is a function that calculates the inverse of a matrix.
## it requires two parameters, one is in the type of matrix and the other one is arbitrary
## this function is calling another built in matrix function solve()
## 

cacheSolve <- function(x=matrix(), ...) {
tmpinv <- x$getinv()
if(!is.null(tmpinv)) {
   message("getting cached data")
   return(tmpinv)
}
data <- x$get()
tmpinv <- solve(data, ...)
x$setinv(tmpinv)
tmpinv
}

###
### This is how to test this program
### For example: if we want to find the inverse of c = rbind(c(1, -1/4), c(-1/4, 1))
### > c = rbind(c(1, -1/4), c(-1/4, 1))
### Initialize the cache
### a <- makeCacheMatrix()
### fill the matrix
### > a$set(c)
### Solve this
### y <- cacheSolve(a)
### y is the inverse of c
### Proof it
### c%*%y
### 

