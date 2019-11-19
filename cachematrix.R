## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function gets a matrix from an input
makeCacheMatrix <- function(x = matrix()) {
        inv=NULL
        ##Sets value of the matrix 
        set= function(y){
                x<<-y
                inv<<-NULL
        }
        get=function() x #gets value of matrix 
        setInverse=function(solveMatrix) inv <<-solveMatrix # sets value of inverse of the matrix 
        getInverse=function() inv #gets the value of the inverse of the matrix 
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
#This function takes the previously created matrix and runs an if statement to see if there is a value for the inverse
#If the inverse is not null the message is returned and the inverse is also returned
#If the inverse is null then the solve function is used to find the inverse and that is returned
cacheSolve <- function(x, ...) {
        inv=x$getInverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data=x$get()
        inv=solve(data)
        x$setInverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}