# load standford coreNLP package
library(phrasemachine)
library(tidyverse)

#
# >> extract_nouns <<
# Input: a description string
# Output: all the nouns in the string
#

extract_nouns <- function(description) {
    output =  POS_tag_documents(description)

    output = as.data.frame(output$Document_1)

    output = filter(output, tags == "NN" | tags == "NNP")


    return(output)
}


