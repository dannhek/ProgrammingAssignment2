## Filename:     cachematrix.R
## Author:       Dann Hekman
## Email:        dannhek@gmail.com
## Start Date:   4/26/14
## Initial Commit Date: n/a
## Last Revision:       n/a
## Purpose:      Assignment for Coursera
##				 Johns Hopkins University RPROG-002
##---------------------------
## Revision History:
## 		04/2014 - Created
##				  makeCacheMatrix
##				  cacheSolve
##---------------------------
## Function:     makeCacheMatrix
## Start Date:   4/26/14
## Initial Commit Date: n/a
## Last Revision:       n/a
## Description:  description1
##				 description2
## Parameters:
## 			x (I,REQ) - a matrix to cache and invert
## Returns: 
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {   
				x <<- y
				m <<- NULL
		}
		get <- function() x
		setinverse <- function(solve) m <<- solve
		getinverse <- function() m
		list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
}
##---------------------------
## Function:     cacheSolve
## Start Date:   4/26/14
## Initial Commit Date: n/a
## Last Revision:       n/a
## Description:  description1
##				 description2
## Parameters:
## 			x   (I,REQ) - a matrix to cache and invert
##          ... (I,OPT) - not currently used
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
