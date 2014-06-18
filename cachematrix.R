## The functions makeCacheMatrix and cacheSolve  are used to obtain the inverse of a matrix.
## The calculated inverse is stored in a cache
## If the inverse is to be calculated again and the matrix has not been changed, the cached value is returned.
## Else, the inverse is re-calculated and returned

#this function creates four functions which are set, get, setinverse, and getinverse and takes a matrix as its input

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL

#the inverse of the matrix "i" is initialized to NULL


        set <- function(y) {
                x <<- y
                i <<- NULL
        }


#the set function is used to define matrix x outside the current environment
#the inverse is returned to NULL outside the function's environment in case the matrix was changed


        get <- function() x

#the get function returns matrix x


        setinverse <- function(inverse) i <<- inverse

#The setinverse function defines the inverse of x outside the current environment


        getinverse <- function() i

#The getinverse function returns the inverted matrix.


        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )
}





#this function takes a matrix x as its input and returns the inverse of this matrix
#if the inverse has been calculated before and the matrix was not changed, the inverse is obtaned from the cache

cacheSolve <- function(x, ...) {
        i <- x$getinverse()

#The inverse of the matrix is set as the output of the getinverse function in makeCacheMatrix

        if(!is.null(i)) {
                message("getting cached data")
                return(i)
	}


#If the inverse is not null (which implies that the matrix has not been changed), the inverse is returned from the cache
#In addition a text message is displayed indicating that the inverse was cached.
 
        matrix <- x$get()
        i <- solve(matrix, ...)
        x$setinverse(i)
        i
#if the inverse was null (implying that the vector was changed), the new value of the matrix is obtained from the get function.
#the new matrix is inverted using the solve function and its inverse is returned.


}

