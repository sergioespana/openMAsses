testfunction <- function() {

  dim <- data.frame(matrix(nrow=5,ncol=1))
  
  dim$var1 <- c(1,4,4,3,7)
  dim$var2 <- c(2,4,6,10,2)
  dim$var3 <- c(1,2,2,8,3)
  dim$var4 <- c(2,6,2,2,5)
  
  dim[1] <- NULL
  
  print(dim)
  div <- c(2,4,2,4)
  dim <- apply(dim, 1, function(x) x / div)
  print(dim)
}
testfunction()

