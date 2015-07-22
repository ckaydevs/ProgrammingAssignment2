## These two functions compute the inverse of a matrix, but instead of calculating every time the same inverse, it caches
## the last value & returns it, if the specified matrix is the same.


## makeCacheMatrix takes in an argument which is by default an empty matrix. It returns a list of functions which
## are (i)set(sets the matrix vector), (ii)get (which gives the matrix stored), (iii)setsolve(stores the inverse 
##of the matrix x),and (iv)getsolve(it gets the cached inverse of a matrix for a specified matrix stored in x)

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL
        set<-function(y){
                x<<-y
                s<<-NULL
        }
        get<- function() x
        setsolve <-function(solve) s <<- solve
        getsolve <-function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}

## This function either computes the inverse of a matrix or returns the last computed(cached) value if the
## matrix specified is unchanged.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s<-x$getsolve()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        matrix <- x$get()
        s <- solve(matrix)
        x$setsolve(s)
        s
}
