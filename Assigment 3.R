makeCacheMatrix <- function(x=matrix()) {
      inverse_matrix <- NULL
      set <- function(y) {
            x<<-y
            inverse_matrix <<- NULL
      }
      setinverse <- function(inverse) {inverse_matrix <<- inverse}
      get <- function() {
            x
      }
      getinverse <- function() {inverse_matrix}
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x,...) {
      inverse_matrix <- x$getinverse()
      #print(inverse_matrix)
      if(!is.null(inverse_matrix )) {
            message("getting cached data")
            return(inverse_matrix)
      }
      data <- x$get()
      inverse_matrix <- solve(data,...)
      x$setinverse(inverse_matrix)
      inverse_matrix
}