## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##Function makeCacheMatrix creates list of functions for work with given matrix
## 1.set_matrix - take given matrix
## 2.get_matrix - return matrix
## 3.set_inverted_matrix - take inverted matrix
## 4.get_inverted_matrix - get inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    
    inverted_matrix <- matrix()
    
    set_matrix <- function(entered_matrix) {
        x <<- entered_matrix
        inverted_matrix <<- matrix()
        
    }
    
    get_matrix <- function() {x}
    
    set_inverted_matrix <- function(solve) {inverted_matrix <<- solve}
    
    get_inverted_matrix <- function() {inverted_matrix}
    
    
    list(set_matrix = set_matrix,
         get_matrix = get_matrix,
         set_inverted_matrix = set_inverted_matrix,
         get_inverted_matrix = get_inverted_matrix)

}


## Write a short comment describing this function

## function cacheSolve retrun inverted_matrix for given matrix
## if inverted matrix is already computed, function doesnt calculate anything, just return
## already calculated matrix
## otherwise, function provides caclulation and returns freshly calculated matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverted_matrix <- x$get_inverted_matrix()
    
    #if it is already calculated
    if(!is.na(inverted_matrix[1][1])){
        print("getting cached data")
        return(inverted_matrix)
    }
    #else
    print("calculating...")
    given_matrix <- x$get_matrix()
    inverted_matrix <- solve(given_matrix,...)
    x$set_inverted_matrix(inverted_matrix)
    
    inverted_matrix
       
}
