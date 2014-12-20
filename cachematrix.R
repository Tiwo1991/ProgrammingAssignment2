## By working together the following two functions allow the user
## to: (1) calculate the inverse of an INVERTABLE 
## square matrix and (2) retrieve the results of a previously
## calculated matrix using the built in cache property 
## of the functions.

        ## You can think of the functions as follows:

        ## The first function takes your matrix and assigns it to a new variable. Additionally, it creates three functions that exist
        ## within the environment of the 'makeCacheMatrix' function.

        ## The second function first looks if a previously calculated inverse square matrix is stored somewhere by some other function:  
        ## it is told by 'm <- x#getsolve()' that this data is somewhere and via lexical scoping it searches the global environment for it   
        ## (see the right side top panel of the RStudio UI, it shows what the Global Environment contains) and finds this in the 'makeCacheMatrix' function.
        ## It then returns this stored data to the user (this is the scenario when the if statement is met and an inverse square matrix has been calculated before).
        ## Otherwise, if the stored data is not what is asked of it to retrieve, the second function calculates the inverse of the given square matrix, 
        ## stores the results 'x$setsolve(m)' and has as output the freshly calculated inverse square matrix for the user. 
        ## When the second function is used again the first scenario would play out. 

## First function: 'makeCacheMatrix' takes an invertable square matrix and via 
## the first function called 'set' assigns it to 'x'.
## The 'makeCacheMatrix' function further contains a set of other functions ('get', 'setsolve', 'getsolve')
## which are called by the 'cacheSolve' function based on when an 'if' condition statement is either met or not.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Second function: Based on the condition in the 'if' statement on the third line, the 'cacheSolve' function
## retrieves the previously calculated matrix and returns this value to the user with an additional message. 
## Otherwise, it does the calculation by first: getting the square matrix by calling the 'get' function and assigning this to 'data' (data <- x$get()).
## Second: applying the solve function to 'data' and assigning the results to 'm' (m <- solve(data, ...)).
## Third: 'x$setsolve(m)' is the caching expression of the function. It invokes the 'setsolve' function that the
## 'makeCacheMatrix' function contains and stores the solve function's results there. Fourth: it returns an inverse square matrix.
## Now, the next time the 'cacheSolve' function is used with the same input (argument) the 'if' (if(!is.null(m))) expression will be met since 'm' 
## contains the solved matrix found in the 'makeCacheMatrix' environment. This will then be shown to the user
## with the message "getting cached matrix data". 

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached matrix data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}