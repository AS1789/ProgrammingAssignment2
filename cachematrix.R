## The pair of functions makeCacheMatrix and cacheSolve allow the return of
## the inverse of
## an invertible matrix, from the cache if the inverse is already available
## there, or by inversion using the R function "solve" if it is not.
## Example of use
## >mymatrix<-makeCacheMatrix(matr) ## matr is an invertible matrix
## >cacheSolve(mymatrix)
##   (other code)
## >cacheSolve(mymatrix)
## first use of cacheSolve following makeCacheMatrix will invert
## and return the inverse of matr, subsequent uses will
## return the inverse from the cache

## makeCacheMatrix takes a matrix and returns a list of functions which
## based on the matrix, and the inverse, primarily as input
## to the function cacheSolve

makeCacheMatrix <- function(x = matrix()) {
		inv<-NULL
		set<-function(y) {
			x<<-y
			inv<<-NULL
					}
		get<-function() x
		setinv<-function(minv) inv<<-minv
		getinv<-function() inv
		list(set=set,get=get,setinv=setinv,getinv=getinv)
								}

## cacheSolve takes the output returned by makeCacheMatrix and returns
## the inverse of the matrix, from the cache if it is there, by inversion if not

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv<-x$getinv()
		if(!is.null(inv)){
			message("getting cached inverse")
			return(inv)
					}
		mat<-x$get()
		inv<-solve(mat,...)
		x$setinv(inv)
		inv
## inv, the inverse of the matrix, is returned
						}
