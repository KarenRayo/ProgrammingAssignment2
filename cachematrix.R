## This function calculates the inverse of a square matrix,
## caching for use in later calculations

## The makeCacheMatrix function takes care of creating a special type object,
## which will be used in the second function that is executed (cacheSolve),
## given the creation of the setter and getter functions.


## At this point the special array type object is initialized, 
## just as the set and get methods are defined.
makeCacheMatrix <- function(x = matrix()) {
              n <- NULL
              set <- function(y){
                ## The value of x is assigned to an object in the main environment
                x<<-y
                
                ## Cache cleared given previous execution of cacheSolve
                n<<-NULL
              }
             get <- function()x ## Recovers x from the main environment of makeCacheMatrix
             ## Set and get methods for the inverse matrix
             
            setinverse <- function(inverse) n <<- inverse
            getinverse <- function() n
            
            ## Create a list with the set and get methods obtained
            list(set = set, get = get, setinverse = setinverse, 
                 getinverse = getinverse)
}


## The second function is cacheSolve, which performs the calculation
## of the inverse array associated with the array that is cached of a special type.

cacheSolve <- function(x, ...) {
        ## The set and get methods of the list stored 
        ## in the previous section are used.
  
        n <- x$getinverse()
        if(!is.null(n)){ ## If n is different from null, 
                         ## they get the cache data and return it
          message("getting cache data")
          return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setinverse(n)
  
        ## Return a matrix that is the inverse of 'x'
        n
}
