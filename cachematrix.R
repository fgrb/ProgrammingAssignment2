#El ejemplo es muy similar al ejemplo aportado por el profesor en el enunciado  
#de la asignacion aplica para una matriz no para un vector, como el ejemplo pero de manera muy similar
makeCacheMatrix <- function(x = matrix()) {
      loc_m <- NULL # de Aqui se inicializa en Null 
      set <- function(y) { #Almacena la Matriz
            x <<- y
            m <<- NULL
      }
      get <- function() x #Obtiene la Matriz
      setInv <- function(loc_m) m <<- loc_m #Establece el valor para loc_m
      getInv <- function() m  #Recupera el valor de Cache 
      list(set = set, get = get, #devuelve una lista de funciones anidadas dentro de makeCacheMatrix
           setInv = setInv,
           getInv = getInv)
}
#La primera Ejecucion no hay nada despues ya salen los datos por ya hay cache
cacheSolve <- function(x) { 
      locmat<- x$get()    # Se obtiene el valor.
      if(!is.null(locmat)) { # .Verifica si hay datos  
            message("Obtener datos de Cache...")  # si hay datos entonces lo indica y los retorna.
            return(locmat)
      }                                
      data <- x$get() # Si no encontro datos toca cargarlos llamando a la funcion x$get de makeCacheMatrix                          
      inversa <- solve(data) # Usa la funcion solve() para obtener la inversa de la matriz.
      #http://www.ub.edu/stat/docencia/EADB/Curso%20basico%20de%20R-bn.pdf
      x$setInv(inversa)  # llama a la funcion x$setInv en makeCacheMatrix 
      inversa  #Devuelve la inversa si no es nula.
}

#Verificaciones o Validaciones 
#Crea la Matriz
x = rbind(c(10,20,30), c(40, 50,60),c(70,80,90),c(1/4,1/5,1-8))
#Imprime que creo
x
# LLama a makeCacheMatrix y le pasa la matriz
m = makeCacheMatrix(x)
m$get()
#Llama a la funcion cacheSolve para  
cacheSolve(m)
