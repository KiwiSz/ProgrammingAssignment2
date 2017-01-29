## makeCacheMatrix is a function which takes a matrix as its argument
## and then creates a list containing that matrix and space in memory to
## store  both it and its inverse. cacheSolve is a function which takes the list 
## created by makeCacheMatrix as its argument and checks if the matrix
## has been solved or not. If so, it returns the stored value from 
## MakeCacheMatrix. If not, it calculates the inverse of the original matrix
## used in the makeCacheMatrix argument and stores it in the solved_matrix
## variable of makeCacheMatrix. This code is based heavily on the code provided in 
## the instructions section of this assignment. It has been modified to satisfy
## the requirements of this assignment. I am not passing it off as original. 

## makeCacheMatrix takes a matrix and returns a list
## containing functions which can modify the orignal 
## matrix and store its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    ## sets the value of s to NULL to clear any existing values of s
    s = NULL
  ## function which stores the value of y in s within the parent 
  ## environment and sets s in the parent environment to null. 
  set = function(y) {
    x <<- y
    s <<- NULL
  }
  ## stores the value of the matrix given as an argument for 
  ## makeCacheMatrix 
  get = function() x
  ## takes the solved matrix as its argument and assigns it to s 
  ## in the parent environment
  solved_matrix = function(solved_matrix) s <<- solved_matrix
  ## stores the value of s 
  holds_matrix = function () s
  ##creates a list assigning all of the functions to names
  ## so that they are accessible and modifiable in subsequent
  ## functions. 
  list(set = set, get = get, 
       solved_matrix = solved_matrix, 
       holds_matrix = holds_matrix)
}
## cacheSolve calls the list created by makecacheMatrix
## and either returns the cached value of the inverse
## of the matrix used in makeCacheMatrix() or 
##calculates its inverse. 
cacheSolve <- function(x, ...){
       ##assigns the matrix from makeCacheMatrix to s
       s = x$holds_matrix()
       ## checks to see if s is empty or not. If not
       ## it returns the value of s 
       if(!is.null(s)) {
         message("matching cache exists")
         return(s)
       }
         ## if s is empty, assigns value of the stored 
         ## matrix from makeCacheMatrix to data
         data = x$get()
         ## finds the matrix inverse and stores it to s
         s = solve(data)
         ## stores s in makeCacheMatrix
         x$solved_matrix(s)
         ## returns solved matrix
         s
}
