## The funtions will return the inverse matrix of a given matrix either by 
##using the function solve() if it's the first time the inverse is calculated
##or by caching the inverse of the matrix if it has already been calculated previously

## The first function, makeCacheMatrix, creates a special matrix that will allow us
##1.Set a matrix (assumed det!=0)
##2.Get the matrix
##3.Set the Inverse Matrix
##4.Get the Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
        MatInversa <- NULL
        set<-function(y){
                x<<-y
                MatInversa<<-NULL
        }
        get<-function() x
        setMinv<-function(Minv) MatInversa<<-Minv #when trying to see if the code works you have to call name$setMinv(solve(name$get()))
        getMinv<-function() MatInversa
        list(set=set,get=get,setMinv=setMinv,getMinv=getMinv)
}


## the next function calculates the inverse of the special matrix created with the function above. 
##it starts by inspecting if the inverse has already been calculated. if it has, it gets the inverse from the cache skiping the computation
##otherwise, it calculates the inverse using the function solver

cacheSolve <- function(x, ...) {
        MatInversa<-x$getMinv()
        if(!is.null(MatInversa)){ # Checks if the inverse matrix has already been calculated
                message("Getting Cached Data")
                return(MatInversa)      #if the condition is satisfied it will return the stored value
        }
        #In case the inverse is yet to be calculated
        Matriz<-x$get()
        MatInversa<-solve(Matriz,...)
        x$setMinv(MatInversa)

        MatInversa ## Return a matrix that is the inverse of 'x'
}
