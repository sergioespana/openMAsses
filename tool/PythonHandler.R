install.packages('reticulate')
library(reticulate)

call_python <- function() 
{
  library(reticulate)
  use_python('C:/Python37')
  python_source = source('C:/Users/Melchior/PycharmProjects/Thesis64/venv/main.py') #TEMP BUILD PATH, CHANGE LATER WHEN INTEGRATING
  #main(program)
  helloworld()
}

call_python()

