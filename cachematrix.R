## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

## Calculating inverse of a matrix is costly.
## Below 2 functions cache the inverse of a matrix.

## makeCacheMatrix function creates a list object containing 1) the matrix, 
## 2) the inverse of the matrix, and 3) functions to set and get both matrices.
makeCacheMatrix <- function(current_matrix = matrix()) {
        inv_matrix <- NULL
        setCurMatrix <- function(new_matrix=matrix()) {
                current_matrix <<- new_matrix
                inv_matrix <<- NULL # when current matrix changes, clear the cached inverse
        }
        getCurMatrix <- function() current_matrix
        setInvMatrix <- function(new_inv_matrix=matrix()) inv_matrix <<- new_inv_matrix
        getInvMatrix <- function() inv_matrix
        list(setCurMatrix = setCurMatrix,
             getCurMatrix = getCurMatrix,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}

## cacheSolve function takes the special list type as input and check if a cached inverse matrix 
## already exists.  If so, the cached inverse matrix is returned, and if not, new inverse matrix
## will be computed, stored, and returned.
cacheSolve <- function(listCacheMatrix) {
        inv_matrix <- listCacheMatrix$getInvMatrix()
        if(!is.null(inv_matrix)) {
                message("getting cached inverse matrix")
                return(inv_matrix)
        }
        message("re-computing inverse matrix")
        current_matrix <- listCacheMatrix$getCurMatrix()
        inv_matrix <- solve(current_matrix)
        listCacheMatrix$setInvMatrix(inv_matrix)
        inv_matrix
}