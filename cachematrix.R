#--------------------------------------------------------------------------
# 
# File : cacheMatrix.R
#
# Functiona : makeCacheMatrix
#               Allows instantion of variable so member functions can be accessed (getting and setting matrix and
#               "solved" value for matrix )     
#               	Example:
#               	1) matrix not assigned :     >w=makeCacheMatrix() ( nothing set ) 
#               	2) Assign matric             >w$set(matrix(c(0, 2, 1, 0, 3,4,5,6,9), nrow = 3, ncol = 3, byrow = TRUE))
#            
#             cacheSolve
#               Will examine the matrix and solve if not cached; otherwise, solve and then cache ( using makeCacheMatrix functions ) 
#                 	EXample ( continuing from above )   > cacheSolve(w)
#                                                    >  [,1]  [,2] [,3]
#						     >  [1,]  0.12 -0.48  0.2
#						     >  [2,]  0.80 -0.20  0.0
#						     >  [3,] -0.60  0.40  0.0
#  						     > cacheSolve(w)
#						     > getting cached data
#                                                    >  [,1]  [,2] [,3]
#						     >  [1,]  0.12 -0.48  0.2
#						     >  [2,]  0.80 -0.20  0.0
#						     >  [3,] -0.60  0.40  0.0
#                Notice in the second call -- the value has been cached -- ( print message invoked ) - so it doesn't
#                need to recalculated -- just return the value from getSolve
#
#             
#
#
#
#
#----------------------------------------------------------------------

## makeCacheMatrix sets up closures for functions to assign matrix values and return solve of matrices
##  
##

makeCacheMatrix <- function(x = matrix()) {
      m<-NULL
      set<-function(y){
         x<<-y
         m<<-NULL
      }
      get<-function() x
      setSolve<-function(solve) m<<- solve
      getSolve<-function() m
      list(set=set, 
           get=get,
           setSolve=setSolve,
           getSolve=getSolve
           )
    
}



## cacheSolve wil check for null for getSolve ( using member functions from
#  makeCacheMatrix )  -- and then return the solved matrix if cachced;
#  otherwise perform solve and set.


cacheSolve <- function(x=matrix(), ...) {
     m<-x$getSolve()
     if(!is.null(m)){
       message("getting cached data")
       return(m)
     }
     matrix<-x$get()
     m<-solve(matrix, ...)
     x$setSolve(m)
     m
}
