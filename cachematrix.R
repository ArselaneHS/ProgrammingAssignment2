## These function will compute and cache the inverse of a given matrix


## This function takes as an argument a matrix x and create and return a special 
## object : it returns a list of functions  who allows you to get and set the  
## value of the matrix and it inverse. 
## It return value will be the argument passed down to the function cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set_mat <- function(y) {  #the function that sets the matrix
                x <<- y
                inv_mat <<- NULL # sets the inverse to NULL in case the value of the matrix has been updated
        }
        get_mat <- function() # the function that gets the matrix
                        x 
        set_inv <- function(inverse) # the function that sets the inverse
                        inv_mat <<- inverse 
        get_inv <- function() #the function that gets the inverse
                        inv_mat 
        list(set_mat = set_mat, get_mat = get_mat,
             set_inv = set_inv,
             get_inv = get_inv)

}


## This function computes and caches the inverse of a matrix. It argument will
## be the return value of makeCacheMatrix() (so that we can set and get the values
## of the matrix and it inverse so that the informations will be cached), if the 
## inverse has been computed it just return it, else it computes the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_mat <- x$get_inv() # getting the value of the inverse
        if(!is.null(inv_mat)) {  #cheking if the inverse has not been already computed
                message("getting cached data")
                return(inv_mat)
        }
        mat <- x$get_mat() #getting the matrix 
        inv_mat <- solve(mat, ...) # computing the inverse
        x$set_inv(inv_mat) # setting the inverse of the matrix
        inv_mat # return(print) the inverse
}


## Cheking that the programme give the inverse of a matrix

mat<- cbind(c(1,2),c(2,1))
mat
mat_func<-makeCacheMatrix(mat)
inv<-cacheSolve(mat_func)
inv
inv%*%mat # this should give us the identity matrix

## ATTENTION : with 3*3 matrices or more exotic values for the matrix, R gives an
## approximation of the inverse, so the mat%*%inv will be a (very precise) 
## approximation of the identity matrix
