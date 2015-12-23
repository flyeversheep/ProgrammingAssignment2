## make CacheMatrix is a function retruning a list of function that will return 
## a list of function. cacheSolve function will use those functions to calculate 
## the inverse of the matrix. If the inverse was originally stored in the cache,
## it will simply output the cache. To use this, please use makeCcheMatrix to 
## generate a matrix then call the cacheSolve to solve the inverse.
## By Yuyang Lao 12/23/2015

## makeCacheMatrix function generate a list containing functions to 
## 1.set the value of the original matrix
## 2.get the value of the original matrix
## 3.set the value of the inverse matrix
## 4.get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL
        set<-function(y){
                x<<- y
                i<<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get=get, 
             setinverse=setinverse,
             getinverse=getinverse
             )
        
}


## This function checks whether the cache data has inverse or not. If not, it 
## will caculate the inverse and store it in the cache and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i<-x$getinverse()
        if(!is.null(i)){
                message('getting cached data')
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}

