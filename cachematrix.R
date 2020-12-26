        ########################################################################
        # This assignment aims to solve the inverse of a matrix by catching the
        # the result within a lexical cope of a function: "makeCacheMatrix" and
        # "cacheSolve". Caching means to use information located in memory
        # to reduce time-consuming computations. Lexical scopes allow to use 
        # nested functions and new objects, defined by user, to save data
        # in different environments
        ########################################################################
        
        ############################################################################
        # the function "makeCacheMatrix" will create a new environment. The inverse.
        # The inverse matrix will be chached inside the object "m", within the main
        # environment, which will be unique for each time that function is invoked
        # the output of the function will be a list with 5 objects (functions):
        # setmatrix, getmatrix, setinverse, getinverse, and getenvironment
        ########################################################################
        
        makeCacheMatrix <- function(x = matrix()) {
                # Example input: Insert matrix e.g x<-matrix(rnorm(100),10,10)
                m<-NULL  # Initialize the inverse property 
                environmen <- environment()  # Save environment
                y<-NULL 
                
                setmatrix<-function(y){  # Set matrix value
                        x<<-y  # cache the matrix - assigns value y from main environment
                        m<<-NULL # search through main environments for an existing definition of the variable and set to NULL
                }
                
                getmatrix<-function() x  # Return the matrix
                setInverse<-function(solve) m<<- solve  # Method to set the inverse of the matrix
                getInverse<-function() m  # Method to get the inverse of the matrix and return the inverse property
                getenvironment<- function() environment()
                
                list (setmatrix=setmatrix, getmatrix = getmatrix, # Return list of the methods  
                      setInverse = setInverse,
                      getInverse = getInverse,
                      getenvironment = getenvironment)
                
        }
        
        ######################################################################
        ## The function "cacheSolve" returns the inverse of the matrix that is 
        # returned by makeCacheMatrix function, e.g. xMat$getmatrix()
        #####################################################################
        
        cacheSolve <- function(xMat= m(), ...) {
                ## Return a matrix that is the inverse of 'x'
                # Run function e.g. like this: minv<-cacheSolve(xMat = m)
                # Compares matrix to what was there before!
                m <- xMat$getinverse() # if an inverse has already been calculated this gets it
                if(!is.null(m)){ # check to see if cacheSolve has been run before
                        if(xMat$setmatrix() == xMat$getmatrix()) { 
                                message("getting cached data")
                                matrix<-xMat$get()
                                m<-solve(matrix, ...)
                                xMat$setmatrix(m)
                                return(m) 
                        }
                        # otherwise 
                        y <- xMat$getmatrix() # run the getmatrix function to get the value of the input matrix
                        xMat$setmatrix(y) # run the setmatrix function on the input matrix to cache it
                        m <- solve(y, ...) # Calculate the inverse using solve function
                        xMat$setinverse(m) # Set inverse to the object
                        m # return the inverse
                }
        }
