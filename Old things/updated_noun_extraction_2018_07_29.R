# test script for fixing POS tagging


noun.input = as.String("the amount of happy CO2 in the atmospheres values")

noun.word.annotation <- NLP::annotate(noun.input, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))

# POS tag the word & extract the word from the output
noun.POSAnnotation<- NLP::annotate(noun.input, Maxent_POS_Tag_Annotator(), noun.word.annotation)

noun.POSwords <- subset(noun.POSAnnotation, type == "word")

noun.tags <- sapply(noun.POSwords$features, '[[', "POS")



noun.tokenizedAndTagged <- data.frame(Tokens = noun.input[noun.POSwords], Tags = noun.tags)



# part 2

# Define a flag(tags_mod) for pos tags - Flag set to 1 if it contains the POS tag we are interested in else 0
# In this case we only want Noun and Adjective tags (NN, JJ)
# Note that this will also capture variations such as NNP, NNPS etc
noun.tokenizedAndTagged$Tags_mod = grepl("NN", noun.tokenizedAndTagged$Tags)

# Initialize a vector to store chunk indexes
noun.chunk = vector()

# Iterate thru each word and assign each one to a group
# if the word doesn’t belong to NN|JJ tags (i.e. tags_mod flag is 0) assign it to the default group (0)
# If the ith tag is in “NN|JJ” (i.e. tags_mod flag is 1) assign it to group i-1 if the (i-1)th tag_mod flag is also 1; else assign it to a new group
noun.chunk[1] = as.numeric(noun.tokenizedAndTagged$Tags_mod[1])
for (i in 2:nrow(noun.tokenizedAndTagged)) {

    if (!noun.tokenizedAndTagged$Tags_mod[i]) {
        noun.chunk[i] = 0
    } else if (noun.tokenizedAndTagged$Tags_mod[i] == noun.tokenizedAndTagged$Tags_mod[i - 1]) {
        noun.chunk[i] = noun.chunk[i - 1]
    } else {
        noun.chunk[i] = max(noun.chunk) + 1
    }

}

# Split and chunk words
noun.text_chunk <- split(as.character(noun.tokenizedAndTagged$Tokens), noun.chunk)
noun.tag_pattern <- split(as.character(noun.tokenizedAndTagged$Tags), noun.chunk)
names(noun.text_chunk) <- sapply(noun.tag_pattern, function(x) paste(x, collapse = "-"))

# Extract chunks matching pattern
# We will extract JJ-NN chunks and two or more continuous NN tags 
# "NN.-NN" -> The "." in this regex will match all variants of NN: NNP, NNS etc
noun.result= noun.text_chunk[grepl("NN|NNS|NNPS", names(noun.text_chunk))]