## Coursera - Rprogramming - Programming assignment 2
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL ##sets the value to NULL as a placeholder for future
        set<-function(y){
                x<<-y
                m<<-NULL
        } ##defines a function to set the vector, x, to a new vector, y, and resets the mean, m, to NULL
        get<-function() x ## returns the vector x
        setmatrix<-function(solve) m<<- solve ##sets the vector to inverse
        getmatrix<-function() m ##returns the vector
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix) ##returns the special 'matrix' object containing allt he functions just defined
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheSolve <- function(x=matrix(), ...) {
                m<-x$getmatrix() ## assign m to cachematrix
                if(!is.null(m)){
                        message("getting cached data")
                        return(m) ## If the vector is not NULL, return it.
                }
                ## If the mean is not stored then:
                matrix<-x$get() ## assign to data the matrix x
                m<-solve(matrix, ...) ## define inverse and assign it to m
                x$setmatrix(m) ## store the inverse matrix x
                m ## return m
        }
