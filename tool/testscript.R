plot <- function(){
  a <- c(1,2,3,6)
  b <- c(2,3,5,7)
  c <- c(2,4,6,8)
  
  print(a/b * 2)
  
  mat <- cbind(a,b,c)
  
  print(which(mat > 2))
  mat[which(mat > 2)] <- 100
  
  print(mat)
}
plot()
rm(plot)
