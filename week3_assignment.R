
##makecachesolve function
makecachematrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solvematrix) inv <<- solvematrix
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse
        )
}

##function to solve or cash matrix inverse
cachesolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("fetching cached data...")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv      
}

##Test 
set.seed(1234)
r<-rnorm(16)
matx<-matrix(r,nrow=4,ncol=4)


test.mtx<-makecachematrix(matx)

notcached<-cachesolve(test.mtx)
print(notcached)

cached<-cachesolve(test.mtx)
print(cached)


