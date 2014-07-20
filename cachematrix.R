## Calculating inverse of a matrix is costly.
## Below 2 functions cache the inverse of a matrix.
## makeCacheMatrix function creates a list object containing 1) the matrix, 
## 2) the inverse of the matrix, and 3) functions to set and get both matrices.
makeCacheMatrix <- function(cur_mtx = matrix()) {
        inv_mtx <- NULL
        setCurMatrix <- function(new_mtx = matrix()) {
                cur_mtx <<- new_mtx
                inv_mtx <<- NULL # when current matrix changes, clear the cached inverse
        }
        getCurMatrix <- function() cur_mtx
        setInvMatrix <- function( new_inv_mtx = matrix()) inv_mtx <<- new_inv_mtx
        getInvMatrix <- function() inv_mtx
        list(setCurMatrix = setCurMatrix,
             getCurMatrix = getCurMatrix,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}

## cacheSolve function takes the special list type as input and check if a cached inverse matrix 
## already exists.  If so, the cached inverse matrix is returned, and if not, new inverse matrix
## will be computed, stored, and returned.
cacheSolve <- function(listCacheMatrix) {
        inv_mtx <- listCacheMatrix$getInvMatrix()
        if(!is.null(inv_mtx)) {
                message("getting cached inverse matrix")
                return(inv_mtx)
        }
        message("re-computing inverse matrix")
        cur_mtx <- listCacheMatrix$getCurMatrix()
        inv_mtx <- solve(cur_mtx)
        listCacheMatrix$setInvMatrix(inv_mtx)
        inv_mtx
}