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

ditiseenfunction <- function(query) {
    #clean text
    cleaned_text = clean_text(query)

    vectorsource = VectorSource(cleaned_text)

    # create a corpus with the source vector for text analysis
    corpus = Corpus(vectorsource)

    # prepare corpus
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords("english")) # remove stopwords
    corpus <- tm_map(corpus, stemDocument)

    tdm = TermDocumentMatrix(corpus)

    return(tdm)
}

steminput <- function(input) {

    output = stemDocument(input)

    return(output)
}


Description_Query1 = 'The emissions of CO2 or equivalent gases made by the organisation. It is usually measured in tonnes.'
Description_Query2 = 'CO2 is a gas that is supposed to cause global warming.'


Description_Query = paste(Description_Query1, Description_Query2)

tdm = ditiseenfunction(Description_Query)

df_tdm = as.data.frame(as.matrix(tdm))


tdm.tfidf = weightTfIdf(tdm)

tm_term_score(tdm.tfidf, steminput("co2"))


## try 1 crap dit kan easier zie shit hierboven

##Descriptions
#Description_Query1 = 'The emissions of CO2 or equivalent gases made by the organisation. It is usually measured in tonnes.'
#Description_Query2 = 'CO2 is a gas that is supposed to cause global warming.'




#tdm.tfidf = weightTfIdf(tdm_combined)

