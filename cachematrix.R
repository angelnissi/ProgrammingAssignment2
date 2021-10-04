setwd('C:/Users/arnavsin/Documents/Coursera-R')
## Put comments here that give an overall description of what your
## functions do
## cacheSolve function calculates the inverse of a matrix that is defined in the
function makeCacheMatrix
## by fetching it through get() function. This inverse is then set in the
function setInverse()
## so anytime same inverse is required, cacheSolve gets the results through the
getInverse() function
## in the cache of the program.
## Put comments here that give an overall description of what your
## functions do
##
makeVector <- function(x = numeric()) {
 m <- NULL
 set <- function(y) {
 x <<- y
 m <<- NULL
 }
 get <- function() x
 setmean <- function(mean) m <<- mean
 getmean <- function() m
 list(set = set, get = get,
 setmean = setmean,
 getmean = getmean)
}
##
cachemean <- function(x, ...) {
 m <- x$getmean()
 if(!is.null(m)) {
 message("getting cached data")
 return(m)
 }
 data <- x$get()
 m <- mean(data, ...)
 x$setmean(m)
 m
}
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 set <- function(y) {
 x <<- y
 m <<- NULL
 }
 get <- function() x
 setinverse <- function(solve) m <<- mean
 getinverse <- function() m
 list(set = set, get = get,
 setinverse = setinverse,
 getinverse = getinverse)
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
 m <- x$getinverse()
 if(!is.null(m)) {
 message("getting cached data")
 return(m)
 }
 data <- x$get()
 m <- solve(data, ...)
 x$setinverse(m)
 m
}
b <- cbind(c(2,1,3), c(0,1,7), c(1,-4,-3))
a <- matrix(b, nrow = 3, ncol = 3)
solve(a)
print(a)
print(makeCacheMatrix(a))
print(cacheSolve(makeCacheMatrix(a)))
print(makeVector(matrix(2, nrow = 3, ncol = 3)))
print(cachemean(makeCache(matrix(2, nrow = 3, ncol = 3))))
