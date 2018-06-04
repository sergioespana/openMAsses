# get content from corpus
# corpus[[1]]$content

# load helper functions from folder to improve code maintanability
source("helperfunctions/googlesheetshelper.r")
source("helperfunctions/pdfhelper.r")
source("helperfunctions/inputcleanhelper.r")

# load libraries

library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming


filepath = "D:/Ralph/Documents/Github/openmasses/tool/ozp_textmining/input_pdf/2016 Bath.pdf"
# read pdf

# get raw text from pdf at the file location
raw_text = read_pdf_to_text(filepath)

# clean the raw text: remove lowercase, punctuation and unwanted characters
Description_Query1 = 'The emissions of CO2 or equivalent gasses made by the organisation. It is usually measured in tonnes.'
Description_Query2 = 'CO2 is a gas that is supposed to cause global warming.'

raw_text= Description_Query2

cleaned_text = clean_text(raw_text)

# make a source vector for the cleaned text
vectorsource = VectorSource(cleaned_text)

# create a corpus with the source vector for text analysis
corpus = Corpus(vectorsource)

# prepare corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus,removeWords,stopwords("english")) # remove stopwords
corpus <- tm_map(corpus, stemDocument)

# create term document matrix for use in text analysis
termdocumentmatrix = TermDocumentMatrix(corpus)

# sort topics with the use of tfidf
tdm.tfidf = weightTfIdf(termdocumentmatrix)

tdm.binary = weightBin(termdocumentmatrix) # binary weighting.

# stem the input
# to do: filter whitespace convert to lower and stopwords.
steminput <- function(input) {

    output = stemDocument(input)

    return (output)
}

# set string to use with tfidf

query = "co2"
query2 = "emissions"
query3 = "emiss"
query4 = "electricity"
# score the document #improve comment
tm_term_score(tdm.tfidf, steminput(query2))

tm_term_score(tdm.binary, steminput(query2)) 



tm_term_score(tdm.tfidf, steminput(query4))

tm_term_score(tdm.binary, steminput(query4))

as.data.frame(as.matrix(corpus))

DF <- full_join(test_df1, test_df2)

table2 = table(strsplit(run2, " "))

# replace NA values with zero
DF=DF %>% mutate_all(funs(ifelse(is.na(.), 0, .)))

DF["new"]=rowSums(DF[-1])
##get sums of all the rows
rowSums(DF[-1])

test_df1 = as.data.frame(table1)
test_df2 = as.data.frame(table2)

combined_df = full