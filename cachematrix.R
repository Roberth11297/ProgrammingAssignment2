#Generamos la matriz a calcular
#Esta función, en resumen, crea un objeto "matriz" especial 
#que puede almacenar en caché su inverso 
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
#Obtenemos los resultados
#Esta función calcula la inversa de la "matriz" especial 
#devuelta por makeCacheMatrix anterior. 
#Si ya se ha calculado la inversa (y la matriz no ha cambiado), 
#entonces la solución de caché debería recuperar la inversa de la caché.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
