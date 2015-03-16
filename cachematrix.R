#MakeCacheMatrix makes a list containing a function which can:
#1.  Set value of the matrix
#2.  Get value of the matrix
#3.  set the value of the inverse of the matrix
#4.  Get the value of the inverse of the matrix

# Explanation on inverse matrices can be found here http://www.mathsisfun.com/algebra/matrix-inverse.html
# By Ilya Pistryakov for Coursera R Programming

makeCacheMatrix <- function(x = matrix()) { #replace original "numeric" with matrix

	i<-NULL #get value of matrix
	set<-function(y){#set value of matrix
		x<<-y
		i<<- NULL
	}	
	get<-function() x
	setinv<-function(inverse) i<<- inverse #sets value of inverse
	getinv<-function() i #gets value of inverse
	list(set=set, get=get, 
		setinv=setinv, 
		getinv=getinv)
}
#This function returns cached inverse matrix if exists, otherwise calculates inverse
cacheSolve <- function(x, ...) {
	i<-x$getinv() #getinv method from makeCacheMatrix function
	if(!is.null(i)){#if inverse alredy cached
		message("getting cached inverse matrix")
		return (i) #return cached matrix
	}
	
	message("creating inverse matrix")
	data<-x$get()
	i<-solve(data)
	x$setinv(i)
	i
	
		
}
x=rbind(c(4,7),c(2, 6))#create matrix
m=makeCacheMatrix(x)
m$get()
cacheSolve(m) #first run, will need to create inverse matrix
cacheSolve(m) #second run to validate that cached inverse matrix will be used