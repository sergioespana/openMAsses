plot <- function(){
  a <- sample(1:15, 5)
  b <- sample(1:15, 5)
  c <- sample(c(0,1))
  mt <- (cbind(a,b,c))
  
  print(mean(mt[1,]))
  
  print(mt)
  plot <- plot_ly(x= mt[mt[,3] == TRUE,1], y = mt[mt[,3] == TRUE,2])
  print(plot)
}
plot()
rm(plot)
