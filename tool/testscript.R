plot <- function(){
  a <- ('this is a quick - string to see how the function works')
  b <- strsplit(a, '-', fixed = TRUE)[[1]][2]
  print(b)
  print(b[[1]][2])
}
plot()
rm(plot)
