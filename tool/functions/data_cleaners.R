#
# >> cleanText <<
# Description: This cleans the text and provides stemming if neccessary
# Input: pdf text
# Output: cleaned pdf text
#

clean_text <- function(inputText, stemming) {

    #make a vectorsource for the input pdf
    outputVectorSource = VectorSource(inputText)
    #make a corpus using the vector
    outputCorpus = tm::Corpus(outputVectorSource)
    
    # clean and stem the text using tm_map this removes whitespace punctuation and stopwords.
    outputCorpus <- tm_map(outputCorpus, content_transformer(tolower))
    outputCorpus <- tm_map(outputCorpus, removePunctuation)
    outputCorpus <- tm_map(outputCorpus, content_transformer(gsub), pattern = '\r', replacement = ' ')
    outputCorpus <- tm_map(outputCorpus, content_transformer(gsub), pattern = '\n', replacement = ' ')
    outputCorpus <- tm_map(outputCorpus, removeWords, stopwords("english")) # remove stopwords
    outputCorpus <- tm_map(outputCorpus, stripWhitespace)
    
    #If stemming is needed, set boolean to true
    if (stemming == TRUE) {
      outputCorpus <- tm_map(outputCorpus, stemDocument)
    }
    
    # convert the corpus back to a list.
    # this is done to avoid problems with the existing code. 
    # TODO Use a true term document matrix in the future.
    outputText = get("content", outputCorpus)

    return(outputText)
}