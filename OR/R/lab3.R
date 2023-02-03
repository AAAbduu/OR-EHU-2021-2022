basic.solution <- function(A, b, x){
  # Implement here.
  e <- A[,x]
  return(solve(e,b))
}

A <- matrix(c(2, 1, 1, 8, 1, 1, 2, 1), nrow=2, byrow=TRUE)
b <- c(6, 4)
c <- c(3, 4, 5, 6)
r <- basic.solution(A,b,c(3,4))
r