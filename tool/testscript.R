plot <- function(){
  all_media <- c()
  all_media$'News' <- 'this is a string'
  all_media$'Social Media' <- 'also a string'
  all_media$'Reddit' <- 'more strings'
  
  print(all_media)
}
plot()
rm(plot)
