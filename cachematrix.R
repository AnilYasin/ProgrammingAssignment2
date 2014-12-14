MakeCacheMatriks<- function(A = matrix()) {
        m<-NULL
        set<-function(AR){
                A<<-AR
                m<<-NULL
        }
        get<-function() A
        setmatrix<-function(solve) m<<- solve
        getmatrix<-function() m
        list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}

CacheSolve <- function(A=matrix(), ...) {
        m<-A$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-A$get()
        m<-solve(matrix, ...)
        A$setmatrix(m)
        m
}
