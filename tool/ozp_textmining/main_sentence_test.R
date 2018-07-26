# get content from corpus
# corpus[[1]]$content

# load helper functions from folder to improve code maintanability
source("helperfunctions/googlesheetshelper.r")
source("helperfunctions/pdfhelper.r")
source("helperfunctions/inputcleanhelper.r")
source("helperfunctions/description_cleaner.r")
# load libraries

library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming


filepath = "~/Github/openmasses/tool/ozp_textmining/input_pdf/2016 Bath.pdf"
# read pdf


corpus_from_text_temp <- function() {
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
    corpus <- tm_map(corpus, removeWords, stopwords("english")) # remove stopwords
    corpus <- tm_map(corpus, stemDocument)

    # create term document matrix for use in text analysis
    termdocumentmatrix = TermDocumentMatrix(corpus)
}


# description stuff

description1 = "The emissions of CO2 or equivalent gasses made by the organisation. It is usually measured in tonnes."
description2 = "CO2 is a gas that is supposed to cause global warming."




# clean the raw text: remove lowercase, punctuation and unwanted characters
cleaned_description = clean_text(description1)

description_vector = VectorSource(cleaned_description)

description_corpus = Corpus(description_vector)


description_corpus <- tm_map(description_corpus, content_transformer(tolower))
description_corpus <- tm_map(description_corpus, removePunctuation)
description_corpus <- tm_map(description_corpus, stripWhitespace)
description_corpus <- tm_map(description_corpus, removeWords, stopwords("english")) # remove stopwords
description_corpus <- tm_map(description_corpus, stemDocument)


description_tdm = TermDocumentMatrix(description_corpus)





# dit kan weg? dit is puur voor inspection purposes
description_matrix = tidy(description_tdm)



#use later

# sort topics with the use of tfidf
tdm.tfidf = weightTfIdf(termdocumentmatrix)

tdm.binary = weightBin(termdocumentmatrix) # binary weighting.

# stem the input
# to do: filter whitespace convert to lower and stopwords.
steminput <- function(input) {

    output = stemDocument(input)

    return(output)
}



#use later

# sort topics with the use of tfidf
tdm.tfidf = weightTfIdf(termdocumentmatrix)

tdm.binary = weightBin(termdocumentmatrix) # binary weighting.

# stem the input
# to do: filter whitespace convert to lower and stopwords.
steminput <- function(input) {

    output = stemDocument(input)

    return(output)
}
