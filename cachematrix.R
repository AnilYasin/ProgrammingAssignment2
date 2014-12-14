AYAR<-MakeCacheMatriks()## I created an object to store MakeCacheMatriks function. I was thinking it would be useful in future.

MakeCacheMatriks<- function(A = matrix()) { ## I started to create function. 
        m<-NULL 
        set<-function(AR){ ## I created this function in order to execute "get","set" operations. It basically creates a matrix
                A<<-AR 
                m<<-NULL ## <<- operator was utilized for assigning a variable outside of current environement
        }
        get<-function()  
        setmatrix<-function(solve) m<<- solve ## set the value of matrix. B
        getmatrix<-function() m ## get the value of matrix.
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

CacheSolve <- function(A=matrix(), ...) { ## I created this function in order to assign values into "A" or indirectly MakeCacheMatriks
        m<-A$getmatrix()
        if(!is.null(m)){  ##This argument checks whether "m" has a value or not. If it doesn't empty, it generates "getting 
                          ##cached data message. In addition, it will return the "m" value.
                message("getting cached data") 
                return(m)
        }
        matrix<-A$get()
        m<-solve(matrix, ...)
        A$setmatrix(m)
        m
}
