## This code has two functions hich cache the inverse of a matrix. 

## This function is named makeCacheMatrix. Input for this function is a matrix. 
#This function- 1. Sets the value of the matrix, 2. Gets the value of the matrix 
#3. Sets the value of inverse matrix and 4. Gets the value of inverse matrix. 


makeCacheMatrix <- function(x = matrix()) {
  a <- NULL 
  # 1. Setting the value of matrix
  set <- function(y) {
    x <<- y 
    a <<- NULL 
  }
  #2. Getting the value of matrix 
  get <- function() x
  #3. Setting the value of the inverse matrix 
  setinverse <- function(inverse) a <<- inverse 
  #4. Getting the value of the inverse matrix 
  getinverse <- function() a
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## This function is named cacheSolve. This function uses the output of the above
#function as input and computes the inverse of the matrix. If the inverse has 
#already been calculated once before then it returns a message - "getting cached
#data" along with the answer. 

cacheSolve <- function(x, ...) {
  
  #Getting the value of the inverse of the matrix from the above function and 
  #returning the inverse matrix if it is not an empty set 
         a <- x$getinverse()
         if(!is.null(a)){
            message("getting cached data")
            return(a)
         }
  #If the value is null then it gets the original matrix, and solves it to 
  #find the inverse of the matrix 
         data <- x$get()
            a <- solve(data, ...)
         x$setinverse(a)
         a
}

# We can test the validity fo our functions by using a 3 by 3 matrix. 
testing <- matrix(c(4, 5, 2, 1, 3, 4, 5, 1, 3),3,3)
CacheMatrix <- makeCacheMatrix(testing)
CacheMatrix$get()
cacheSolve(CacheMatrix)