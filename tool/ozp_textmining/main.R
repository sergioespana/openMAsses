# get content from corpus


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

filepath = "input_pdf/2016 Bath.pdf"
# read pdf

# get raw text from pdf at the file location
raw_text = read_pdf_to_text(filepath)

# clean the raw text: remove lowercase, punctuation and unwanted characters
Description_Query1 = 'The emissions of CO2 or equivalent gasses made by the organisation. It is usually measured in tonnes.'
Description_Query2 = 'CO2 is a gas that is supposed to cause global warming.'


corpus1 = process_input_to_Corpus(Description_Query1)
corpus2 = process_input_to_Corpus(Description_Query2)
corpus_total_text = process_input_to_Corpus(raw_text)

corpus_combined = Corpus(VectorSource(
  mapply(function(x, y) paste(content(x), content(y)), corpus1, corpus2)
))

# create term document matrix for use in text analysis
termdocumentmatrix = TermDocumentMatrix(corpus_combined)

termdocumentmatrix_all_text = TermDocumentMatrix(corpus_total_text)

# sort topics with the use of tfidf
tdm.tfidf = weightTfIdf(termdocumentmatrix)

tdm.binary = weightBin(termdocumentmatrix) # binary weighting.



tdm1.tfidf = weightTfIdf(TermDocumentMatrix(corpus1))
# stem the input
# to do: filter whitespace convert to lower and stopwords.
steminput <- function(input) {

    output = stemDocument(input)

    return (output)
}


# input: raw text such as a string. no arrays yet
# output: cleaned raw text as a corpus
process_input_to_Corpus <- function(raw_input) {
    #clean input first
    clean_input = clean_text(raw_input)

    #make a vector source for the cleaned text
    vectorsource_input = VectorSource(clean_input)

    #make acorpus with the source vector for analysis
    corpus_output = Corpus(vectorsource_input)

    corpus_output <- tm_map(corpus_output, content_transformer(tolower))
    corpus_output <- tm_map(corpus_output, removePunctuation)
    corpus_output <- tm_map(corpus_output, stripWhitespace)
    corpus_output <- tm_map(corpus_output, removeWords, stopwords("english")) # remove stopwords
    corpus_output <- tm_map(corpus_output, stemDocument)
}

# set string to use with tfidf

query = "co2"
query2 = "emissions"
query3 = "emiss"
query4 = "electricity"
# score the document #improve comment
tm_term_score(tdm.tfidf, steminput(query2))

tm_term_score(tdm_total_text.tfidf, steminput(query))

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