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


filepath = "test_files/2016 Bath.pdf"
# read pdf

# get raw text from pdf at the file location
raw_text = read_pdf_to_text(filepath)

# clean the raw text: remove lowercase, punctuation and unwanted characters
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
