## Queste funzioni creano un oggetto matrice speciale che può
## memorizzare nella cache la propria matrice inversa.

## Crea un oggetto matrice con funzioni per impostare e recuperare
## la matrice e la sua inversa memorizzata.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  getinverse <- function() inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Restituisce l'inversa della matrice.
## Se l'inversa è già stata calcolata, la recupera dalla cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}