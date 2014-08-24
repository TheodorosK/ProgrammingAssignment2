## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than computing it repeatedly 
## The below code defines two user defined functions (UDFs)
## that cache the inverse of a matrix

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix if and only if the matrix is symmetric/square (i.e. it can be solved) 
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  ## Initialization of the variable that will hold the inverse matrix
  s <- NULL
  
  ## We define the Set Method of the function
  ## the method will try if the given matrix can be solved (i.e. inversed)
  ## and if not, it will produce a warning to the user
  ## if the matrix is a square matrix, then the method will cache the value of the matrix
  set <- function(y) {
    ## the tryCatch method will try to test if the inverse of the given matrix multiplied by the original matrix
    ## will produce an error. Normally, the inverse of a matrix when multiplied by the original matrix will
    ## produce the identity matrix. If the matrix cannnot be solved (i.e. inversed) then an error will be created and 
    ## the trycatch method will catch the error
    out <- tryCatch(solve(y) %*% y, error = function(e) e)
    
    ## if there is any error then stop then do not set anything and produce a warning to the user
    ## else set the value of the matrix in the parent/global environment (i.e. cache the value)
    if (any(class(out)=="error")) {
      warning("Matrix is not symmetric. Please try creating a symmetric matrix.")  
    } else {
      x <<- y
      s <<- NULL
    }    
  }  
  ## we define the get method. This method will return the value of the matrix
  get <- function() x
  ## we define the setSolved method. This method will cache to the parent/global environment 
  ## the value of the inversed (i.e. solved) matrix
  setSolved <- function(solved) s <<- solved
  ## we define the getSolved method. This method will return the value of the cached inversed matrix
  getSolved <- function() s
  ## finally, we return the list of methods defined for the makeCacheMatrix function
  list(set=set, get=get, setSolved=setSolved, getSolved=getSolved)
}

## The cacheSolve function calculates the inverse of the special "matrix" created 
## with the makeCacheMatrix function. It first checks to see if the inversed has 
## already been calculated. If so, it gets the inversed matrix from the cache and skips 
## the computation. Otherwise, it calculates the inverse matrix and sets 
## the value in the cache via the setSolved function.
## we also round the number of digits to 2 for the inversed matrix for more clarity when 
## checking the results

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getSolved()
  ## if the inversed has been calculated, then retrieve the results from the cache
  ## else calculate the inverse of the given matrix, set the results to the cache
  ## and print out the results
  if (!is.null(s)){
    message("getting cached matrix")
    return(s)
  } else {
    m <- x$get()
    s <- round(solve(m),2)
    x$setSolved(s)
    s
  }
}


## Illustrated example:
##
## > matrix <- round(matrix(rnorm(100),10,10),2)
## > m<-makeCacheMatrix(matrix)
## > m$get()
## [,1]  [,2]  [,3]  [,4]  [,5]  [,6]  [,7]  [,8]  [,9] [,10]
## [1,] -0.48  0.74 -0.36  0.01 -0.32 -0.24  0.74  1.39  1.83  0.81
## [2,] -0.79 -0.97  0.38 -0.37 -0.79 -2.03 -0.62 -0.04 -0.85  0.88
## [3,]  0.57  0.30  1.00 -0.28 -1.05 -1.04 -1.02  0.25  0.23  0.15
## [4,]  0.30  1.44  0.52  2.39 -0.98 -0.94  1.20 -0.72  0.21 -0.82
## [5,]  0.23 -0.33 -0.03  1.50  0.13  1.43  0.77 -0.62 -2.09 -1.06
## [6,]  0.79  0.58  1.54  0.24 -0.45  0.97 -0.26  0.67 -1.61  1.45
## [7,] -2.24 -0.15 -0.73  1.16  1.11 -0.48 -0.19  0.79 -1.88  0.42
## [8,]  0.25  0.70  0.91  1.14  0.34  0.19 -2.63 -0.42  0.59 -1.20
## [9,] -0.56  1.34 -1.13  1.06  2.05  0.57  0.07 -0.34 -0.05  1.51
## [10,] -0.68  0.13 -0.28  0.67  0.94  1.12  0.34  0.28 -0.73 -0.41
##
## we now call the cacheSolve function (remember: we haven't calculated the inverse of the matrix yet)
## The call of the cacheSolve function will check the the inversed hasn't been calculated yet
## and will calculate, set it and then print it out.
##
## > cacheSolve(m)
## [,1]   [,2]   [,3]  [,4]  [,5]   [,6]   [,7]  [,8]  [,9]  [,10]
## [1,]  6.81  15.45  28.85 -4.65 11.77 -10.35 -16.72 -4.61 10.52  34.57
## [2,] -5.78 -13.27 -21.50  3.71 -9.31   8.04  13.65  3.21 -8.07 -27.81
## [3,]  2.48   7.15  10.91 -1.43  3.77  -3.77  -7.44 -1.69  3.97  15.95
## [4,]  3.86   8.23  13.59 -2.20  6.13  -5.03  -8.42 -1.88  5.22  17.10
## [5,]  7.32  18.13  32.11 -4.96 12.32 -11.64 -19.29 -5.16 11.85  41.02
## [6,] -4.30 -10.57 -19.45  2.86 -7.39   7.05  11.12  3.21 -6.99 -23.74
## [7,]  1.85   4.78   8.10 -1.05  3.00  -2.95  -5.14 -1.57  2.95  10.99
## [8,]  5.37  11.45  21.76 -3.54  8.69  -7.71 -12.20 -3.46  7.63  26.20
## [9,]  1.18   2.43   3.02 -0.52  1.34  -1.29  -2.52 -0.30  1.29   4.73
## [10,] -0.36  -1.11  -3.23  0.35 -0.93   1.23   1.43  0.57 -0.79  -3.75
##
## We now try to call again the cacheSolve function. Recall that we have now calculated the inverse.
## The function will now check if the inverse already exists. As we have already calculated it,
## it will not re-calculate it but will simply get the results from the cache.
##
## > cacheSolve(m)
## getting cached matrix
## [,1]   [,2]   [,3]  [,4]  [,5]   [,6]   [,7]  [,8]  [,9]  [,10]
## [1,]  6.81  15.45  28.85 -4.65 11.77 -10.35 -16.72 -4.61 10.52  34.57
## [2,] -5.78 -13.27 -21.50  3.71 -9.31   8.04  13.65  3.21 -8.07 -27.81
## [3,]  2.48   7.15  10.91 -1.43  3.77  -3.77  -7.44 -1.69  3.97  15.95
## [4,]  3.86   8.23  13.59 -2.20  6.13  -5.03  -8.42 -1.88  5.22  17.10
## [5,]  7.32  18.13  32.11 -4.96 12.32 -11.64 -19.29 -5.16 11.85  41.02
## [6,] -4.30 -10.57 -19.45  2.86 -7.39   7.05  11.12  3.21 -6.99 -23.74
## [7,]  1.85   4.78   8.10 -1.05  3.00  -2.95  -5.14 -1.57  2.95  10.99
## [8,]  5.37  11.45  21.76 -3.54  8.69  -7.71 -12.20 -3.46  7.63  26.20
## [9,]  1.18   2.43   3.02 -0.52  1.34  -1.29  -2.52 -0.30  1.29   4.73
## [10,] -0.36  -1.11  -3.23  0.35 -0.93   1.23   1.43  0.57 -0.79  -3.75
##
## Now we check the validity of the inverse matrix. In order to do that, 
## we will multiply the inverse with the original matrix in order to get 
## the idendity matrix (a matrix with all zeros, except the diagonal which is all 1s)
##
## > inversed <- cacheSolve(m)
## getting cached matrix
## > round(inversed%*%matrix,1)
## [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
## [1,]    1    0    0    0    0    0    0    0    0     0
## [2,]    0    1    0    0    0    0    0    0    0     0
## [3,]    0    0    1    0    0    0    0    0    0     0
## [4,]    0    0    0    1    0    0    0    0    0     0
## [5,]    0    0    0    0    1    0    0    0    0     0
## [6,]    0    0    0    0    0    1    0    0    0     0
## [7,]    0    0    0    0    0    0    1    0    0     0
## [8,]    0    0    0    0    0    0    0    1    0     0
## [9,]    0    0    0    0    0    0    0    0    1     0
## [10,]    0    0    0    0    0    0    0    0    0     1