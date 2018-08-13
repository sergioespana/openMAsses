# init stuff
require(tidyverse)
backup_output = output
source("functions/keyword_generation.R")


# get topic name
topic_name = str_extract(toString(output[2, 1]), "[^;]*$")

