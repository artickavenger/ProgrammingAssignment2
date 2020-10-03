## Purpose of this Function is to Cache the Matrix Inverse
## Usually Matrix inversion takes more time in computation and there may some 
## benefit to caching the inverse rather than compute it directly.

## This function contain 2 separate functions that are used to create special 
## object that stores a matrix object that can caches its inverse.


makeCacheMatrix <- function(x = matrix()) {
        nll <- NULL
        set <- function(y) {
                x <<- y
                nll <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) nll <<- inverse
        getInv <- function() nll
        list(set = set,
             get = get,
             setInv = setInv,
             getInv = getInv)
}



## This function use to computes the matrix object produced by the 
## makeCacheMatrix function above. If it is already been calculated (and the 
## matrix has not changed), then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        get <- x$get()
        inv <- solve(get, ...)
        x$setInv(inv)
        inv        
}

## Function Evaluation
### I include sample evaluation file to test the function that I made,
### just remove the "#" if you want to test it.
# mat <- makeCacheMatrix(matrix(2:5, 2,2))

# mat$get()
### get() function will return the following
###       [,1] [,2]
### [1,]    2    4
### [2,]    3    5

# mat$getInv() 
### NULL

# cacheSolve(mat)
### This function will returned the inverse of mat
###      [,1] [,2]
### [1,] -2.5    2
### [2,]  1.5   -1

# cacheSolve(mat)
### getting cached data
###      [,1] [,2]
### [1,] -2.5    2
### [2,]  1.5   -1

# mat$getInv()
### repeating this function will now return the inverse of mat
###       [,1] [,2]
### [1,] -2.5    2
### [2,]  1.5   -1
