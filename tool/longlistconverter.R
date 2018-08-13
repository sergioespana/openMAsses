# help script for converting a longlist V1 to a longlist

# load libraries

require(readxl)
require(tidyverse)

input = read_excel("long-listV1.xlsx")

input.data.frame = as.data.frame(input)

input.type = typeof(input.data.frame)


input = read.csv("long-listV1.csv")

df = as.data.frame(input)

test.separate = separate(input,Categories,sep = '/')