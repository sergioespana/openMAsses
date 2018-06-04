#load helper functions
source("helperfunctions/googlesheetshelper.r")
source("helperfunctions/pdfhelper.r")
source("helperfunctions/inputcleanhelper.r")

# load libraries
library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming

