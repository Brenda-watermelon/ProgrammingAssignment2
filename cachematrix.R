#https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#We want to work on a MATRIX vector, in the function we must set x = matrix(). 
#We are basically telling our function the "primary data" we are going to work with.

makeCacheMatrix <- function(x = matrix()) {
    #In hierarchy order
    #set() environment. 
    inv <- NULL            #must be NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    #get() environment. x is given in function(x = matrix())
    get <- function(){
        x
    }
    
    #set_Inverse() environment
    set_Inverse <- function(inverse){
        inv <<- inverse
    }
    
    #get_Inverse() environment. This will show what we have as value inv.
    get_Inverse <- function(){
        inv
    }
    
    #This line is necessary to display the results went you call $ each of the functions in makeCacheAMatrix
    list(set = set, get = get, set_Inverse = set_Inverse, get_Inverse = get_Inverse)
}

#------------------------------------------------------------------------------------------------------------------------------------------
#Let's play!

my_matrix <- makeCacheMatrix(matrix(runif (16, 1, 75), 4, 4))
> my_matrix$get()
[,1]     [,2]      [,3]     [,4]
[1,] 43.30838 27.23908  9.269862 12.94571
[2,] 16.49654 60.76769  5.526713 17.20654
[3,] 19.58476 74.33996  2.947350 42.93948
[4,] 57.47070 63.30449 17.592653 42.42507

> my_matrix$get_Inverse()
NULL

#------------------------------------------------------------------------------------------------------------------------------------------

#Picture set() and set_Inverse() as first and second steps
#and get() and get_Inverse() as first and second steps
#set() and get() are also concatenated.





## Write a short comment describing this function
#cacheSolve <- function(x, ...) {

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    
    #the inv is the resoult of an if loop
    inv <- x$get_Inverse()
    if(!is.null(inv)) {
        message("getting inversed matrix")
        return(inv)
    }
    
    primarydata <- x$get()
    inv <- solve(a = primarydata, ...)  #you can omit the a = , I did for myself. inv <- solve(primarydata, ...) this is the same.
    x$set_Inverse(inv)
    inv
    
}               

#ï----------------------------------------------------------------------------------------------------------------------------------------
#Let's play! (using the previous matrix my_matrix)
cacheSolve(my_matrix)
[,1]        [,2]         [,3]        [,4]
[1,]  0.060738363 -0.01897912  0.018003192 -0.02905791
[2,]  0.004710856  0.02795577 -0.001789194 -0.01096476
[3,] -0.154461504  0.06860089 -0.115285603  0.13599349
[4,] -0.025256454 -0.04445140  0.026088055  0.02290180


#ï----------------------------------------------------------------------------------------------------------------------------------------
#should retrieve the inverse from the cache.
