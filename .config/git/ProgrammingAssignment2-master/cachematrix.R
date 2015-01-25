## Put comments here that give an overall description of what your
## functions do
makeCacheMatrix <- function(x = matrix()) {
        ##sets the value of the matrix
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        ## get the value of the matrix
        get<-function() x
        ## sets the inverse of the matrix
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        ## gets the inverse of the matrix
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
        ##returns an inverse of the matrix 'x'
        m<-x$getmatrix()
        ## checks if there is a matrix
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        ##if not then gets the inverse of the matrix
        matrix<-x$get()
        m<-solve(matrix, ...)
        ## sets the invers of the matrix
        x$setmatrix(m)
        m
}
##data to test
a<-makeCacheMatrix(matrix(c(1,2,3,2,5,2,6,-3,1), nrow=3, ncol=3))
cacheSolve(a)



