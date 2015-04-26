##
## This R program is a part of assignment 2
## Created by TL based on the skeleton provided in the assignment. 
## 
## makeCacheMatrix is a function which creates a special "matrix" object that can cache its inverse. 
## This function requires one parameter x with object type as matrix. 
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
## cacheSolve is a function which computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## it requires two parameters,
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

### test git push
