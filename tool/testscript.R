plot <- function(){
  a <- (c(10,5,7))
  b <- (c(3,0,10))
  c <- (c(7,2,3))
  mt <- data.frame(cbind(a,b,c))
  
  print(mt)
  meancol <- rowMeans(mt[,2:3])
  print(meancol)
  print(mt$a)
  plot(meancol, mt$a)
}
plot()
rm(plot)
