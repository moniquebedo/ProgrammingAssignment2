##The function will receive as argument a matrix and do all the process
##first function = "caching the data
makeCacheMatrix<-function(x=matrix()){
  inv<-NULL  #placeholder for a future value
  #set the value of the matrix with a function
  set<-function(y){
    x<<-y  #asign the value of the right operator to an object in the parent environment (left side operator)
    inv<<-NULL #clears any value of m that had been cached by a prior execution of cacheInv()
  }
  get<-function(){
    x  #return the matrix
  }
  setInv<-function(inverse){
    inv<<-inverse ## inv is defined in the parent envi, we use <<- to assign the value in the parent envi
  }
  
  getInv<-function(){
    inv
  }
  ##now lets create ours "special vector" that contain all my functions, as requested
  ##give names to each element in the list so we can use $ operator to extract and get the content of the matrix
  list(get=get, set=set, setInv=setInv, getInv=getInv)
}

#Rettrieve the matrix from object above
cacheSolve<- function(x, ...){ #... will allow the caller add more arguments
  #lets try to retrieve the inverse of the argument of this function
  
  inv<-x$getInv()
  #we check if the value is null
  #if we have a valid, cached the inverse we can return it to the parent env
  if(!is.null(inv)){
    message("getting data")
    return(inv)
  }
  #otherwise, calculate the inverse
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInv(inv)
  return(inv)
  
}

