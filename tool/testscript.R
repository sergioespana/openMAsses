plot <- function(){
  a <- c(1,2,3,6)
  b <- c(2,3,5,7)
  c <- c(2,4,6,8)
  
  mat <- data.frame(cbind(a,b,c))
  
  colnames(mat) <- c('a','b','c')
  print(mat)
}
plot()
rm(plot)
