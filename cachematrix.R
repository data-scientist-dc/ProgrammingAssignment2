## The First function creates the four functions and returns those four functions along with the enviroment details.
## The Second function uses the first set of functions to cache the inverse of a matrix, if already exists. If not, it calcualtes the inverse and caches it

## Creates four functions and returns them along with the environment details

makeCacheMatrix <- function(x = matrix()) {
        # "I" variable (which will be used to store the matrix inverse) is initialized to Null so as to not have stale values
        I <- NULL
        #set function stores the argument(y) passed to it in the variable 'x' that exists in the parent environment and also initializes the "I" variable (again from the parent environment) to NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        # get function returns the variable 'x'
        get <- function() x
        #setInv function stores the argument(inverse) passed to it in the variable "I" that exists in the parent environment 
        setInv <- function(inverse) I <<- inverse
        #getInv function returns the variable "I"
        getInv <- function() I
        # the function returns a list of the four functions, each with the name same as the function name
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## The Second function uses the first set of functions to cache the inverse of a matrix, if already exists. If not, it calcualtes the inverse and caches it

cacheSolve <- function(x, ...) {
        # calls the 'getInv' variable/function from the list x and stores the value returned from the function call in "I"
        I <- x$getInv()
        #checks if the value of "I" is not Null in which case the stored value is returned along with the message and the function is exit
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        # If the value of "I" is Null, the run continues and calls the 'get' variable/function fromthe list x and store its value in 'data' variable
        data <- x$get()
        # get the inverse of the 'data' variable and store it in I
        I <- solve(data, ...)
        # call the setInv variable/function of list x with the argumnet I (newly created inverse)
        x$setInv(I)
        # returns the newly created inverse
        I
}
