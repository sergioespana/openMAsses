plot <- function(){
  a <- sample(1:15, 3)
  b <- sample(1:15, 3)
  c <- sample(c(0,1,4))
  mt <- cbind(a,b,c)
  
  print(mt)
  vec <- c(2,0,1)
  mt<- sweep(mt, 2, vec, '*')
  print(mt)
}
plot()
rm(plot)
