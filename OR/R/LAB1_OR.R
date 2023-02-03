#1- Basic Data Structure
#Exercise 1
x<-c(1,3,5,7)
y<-c(2,4,6,8)
z<-c(7,5,3,1)
t<-c(8,6,4,2)
#Exercise 2
p<-z-x
p
p < -3
which(p < -3)
all(p < -5)
any(p < -5)
#Exercise 3
m <- matrix(c(x,y,z,t), nrow = 4, byrow=FALSE)
m
mSum <- matrix(c(x,y,(y+z),t), nrow = 4, byrow=FALSE)
mSum
mSum>4
#Exercise 4
myList <- list(x,y,m,"Abdu")
myList
myList[[2]]
myList[[3]][2,3]
#2- Functions
#Exercise 1
x<-seq(from=5, to=1, by=-1)
x
y<-order(x)
y
y<-append(y,c(2,4))
y
z<-unique(y)
z
#Exercise 2
i<- seq(10, 100, 1)
result <- sum(i^2+4*i^5)
result
#Exercise 3
x <- c(1, 4, 2, 4, 6, 1)
mean(x)
myMean <- function(vec){
  s<-sum(vec)
  return (s/length(vec))
}
myMean(x)
#Exercise 4
my2Transpose <- function(m,n){
  return(t(m*n))
}
my2Calculator <- function(m,n){
  algeP <- t(m %*% n)
  sAboveD <- upper.tri(algeP)
  rList <- list(AlgebraicProduct=algeP, Rows=nrow(algeP), Columns = ncol(algeP), SAboveDia=sAboveD)
  return (rList)
}
#Exercise 5
#Function to create data structure to contain matrix A, vector b and objective function c.
createDS <- function(m,f){
ci  <-m[,ncol(m)]

m[,ncol(m)]<-c(0*nrow(m))
probl <- list(A=m,b=t(ci),c=f)

varRow <- c("x1")
for(i in 1:ncol(probl$A)){
  if(i==ncol(probl$A)){
    varRow<-c(varRow, "IC")
  }else{
  varRow<-c(varRow, paste(c("x", i), collapse = ""))
  }
}
probl$A <- rbind(probl$A, varRow[-1])

return (probl)
}


#Given linear model in the pdf
fu<-c(3,4,5,6)
e1 <- c(2,1,1,8,6)
e2<- c(1,1,2,1,4)
mA <- matrix(c(e1,e2), nrow=2, byrow=TRUE)
mA
#LINEAL MODEL PDF END

#Function to extract each of the columns of the matrix, returns a list with the columns of the matrix which is in the given data as parameter.
prepareData <- function(dataI){
  vec <-c()
  columns<-list()
  for(i in 1:ncol(dataI$A)){
    if(i<ncol(dataI$A)){
      for(e in dataI$A[,i]){
        vec <- c(vec,e)
      }
     columns[[i]] <- vec
     vec <-c()
    }
  }
  return(columns)
  
}


#Function to solve and get a basic solution.
basicSolution <- function(m, data){
  return(solve(m,t(data$b)))
}


#Function that given a matrix (A|b) type and an objective function, given as a vector, solves and prints all possible basic solutions.
getBasicSolutions <- function(mA, fu){
  
  #Creating data structure to manipulate matrix A, vector b and objective function c.
  data <- createDS(mA,fu)
  data
  
  #Function to extract each of the columns of the matrix
  preparedData<-prepareData(data)
  preparedData
  
  #Combination without repetition of the columns of matrix A, in order to get all basic possible solutions.
  combin <- combn(x=preparedData,m=length(data$b),simplify = FALSE)
  
  for(i in 1: length(combin)){
    
    exV <- combin[[i]]
    exV <- unlist(exV)
  
    
    exM <-matrix(data=exV,nrow = length(data$b)+1,ncol = length(data$b),byrow=FALSE)
   
    #save the solution coefficients x0..xn to know which solutions we are refering.
    x_c <- tail(x=exM, n=1)
    
    #candidateM is a matrix which might be a candidate to be solved if the vectors forming it are linearly independent (they for a basis)
    candidateM <- head(x=exM, n=length(data$b))
    candidateM <- matrix(as.numeric(candidateM), ncol=ncol(candidateM))
    
    #if linearly independent, solve
      if(det(candidateM)!=0){
        basicS <- basicSolution(candidateM,data)
        if(all(basicS>=0)){
          basicS <- cbind(t(x_c),basicS)
          print(basicS)
        }
      }
  }
  print("Remember that those variables that do not appear in the solution take the value 0!")
}

getBasicSolutions(mA,fu)

 
