require(tidyverse)

# https://www.r-bloggers.com/key-phrase-extraction-from-tweets/ 
require(NLP)
require(openNLP)
require(phrasemachine)
require(tidyverse)
require(corpus)
require(SnowballC)



# input: a sentence string that describes a topic
# output: a list with keywords that uses nouns and adjectives tuple



ozp_generate_keywords_nouns_adjectives <- function(input.noun.adj,mode) {
    #input.noun.adj = "sustainability research"
    # input = test_input
   # input.noun.adj=test.noun
    
    input.noun.adj <- as.String(input.noun.adj)
    # Before POS tagging, we need to do Sentence annotation followed by word annotation
    wordAnnotation.noun.adj <- NLP::annotate(input.noun.adj, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))

    # POS tag the words & extract the "words" from the output
    POSAnnotation.noun.adj <- NLP::annotate(input.noun.adj, Maxent_POS_Tag_Annotator(), wordAnnotation.noun.adj)
    POSwords.noun.adj <- subset(POSAnnotation.noun.adj, type == "word")
    # Extract the tags from the words
    tags.noun.adj <- sapply(POSwords.noun.adj$features, '[[', "POS")

    # Create a data frame with words and tags
    tokenizedAndTagged.noun.adj <- data.frame(Tokens = as.character(input.noun.adj[POSwords.noun.adj]), Tags = tags.noun.adj)

    # part 2

    # Define a flag(tags_mod) for pos tags - Flag set to 1 if it contains the POS tag we are interested in else 0
    # In this case we only want Noun and Adjective tags (NN, JJ)
    # Note that this will also capture variations such as NNP, NNPS etc
    if (mode == 1){
        tokenizedAndTagged.noun.adj$Tags_mod = grepl("NN|JJ", tokenizedAndTagged.noun.adj$Tags)
    }
    else if(mode==2){

        tokenizedAndTagged.noun.adj$Tags_mod = grepl("NN|NN", tokenizedAndTagged.noun.adj$Tags)
    }

    # Initialize a vector to store chunk indexes
    chunk.noun.adj = vector()

    # Iterate thru each word and assign each one to a group
    # if the word doesn’t belong to NN|JJ tags (i.e. tags_mod flag is 0) assign it to the default group (0)
    # If the ith tag is in “NN|JJ” (i.e. tags_mod flag is 1) assign it to group i-1 if the (i-1)th tag_mod flag is also 1; else assign it to a new group

    if (length(tokenizedAndTagged.noun.adj$Tags_mod) > 1) {
        chunk.noun.adj[1] = as.numeric(tokenizedAndTagged.noun.adj$Tags_mod[1])
        for (i in 2:nrow(tokenizedAndTagged.noun.adj)) {
            test.value = i
            if (!tokenizedAndTagged.noun.adj$Tags_mod[i]) {
                chunk.noun.adj[i] = 0
            } else if (tokenizedAndTagged.noun.adj$Tags_mod[i] == tokenizedAndTagged.noun.adj$Tags_mod[i - 1]) {
                chunk.noun.adj[i] = chunk.noun.adj[i - 1]
            } else {
                chunk.noun.adj[i] = max(chunk.noun.adj) + 1
            }

        }
    } else {
        chunk.noun.adj[1]=1
    }

    # Split and chunk words
    text_chunk.noun.adj <- split(as.character(tokenizedAndTagged.noun.adj$Tokens), chunk.noun.adj)
    tag_pattern.noun.adj <- split(as.character(tokenizedAndTagged.noun.adj$Tags), chunk.noun.adj)
    names(text_chunk.noun.adj) <- sapply(tag_pattern.noun.adj, function(x) paste(x, collapse = "-"))

    # Extract chunks matching pattern
    # We will extract JJ-NN chunks and two or more continuous NN tags 
    # "NN.-NN" -> The "." in this regex will match all variants of NN: NNP, NNS etc
    if (mode == 1) {
        result.noun.adj = text_chunk.noun.adj[grepl("JJ-NN|NN.-NN", names(text_chunk.noun.adj))]
    } else {
        result.noun.adj = text_chunk.noun.adj[grepl("NN|NN", names(text_chunk.noun.adj))]
    }
    

    outputs.noun.adj = list()


    i = 1
    j = length(result.noun.adj)
    while (i <= j) {

        outputs.noun.adj[i] = paste(result.noun.adj[[i]],collapse= " ")
       
        i = i + 1

    }

    # start building the result
    result.noun.adj = list()

    if (length(outputs.noun.adj) > 0) {

        for (nounadj in 1:length(outputs.noun.adj)) {
            # retrieve the keyword pair
            keyword.noun.adj = outputs.noun.adj[nounadj]

            # collapse the keyword strings into one
            keyword.noun.adj = paste(keyword.noun.adj[[1]], collapse = " ")

            ## stem the keyword pair
            # split the words and stem those words individually to avoid stemming errors.
            keyword.noun.adj.splitted = str_split(keyword.noun.adj, " ")
            # stem the  splitted words and combine them into one string
            keyword.noun.adj.stemmed = paste(SnowballC::wordStem(keyword.noun.adj.splitted[[1]], language = "english"), collapse = " ")

            # append to the result list
            result.noun.adj[nounadj] = keyword.noun.adj.stemmed
        }
    }

    return(result.noun.adj)
}

# load standford coreNLP package

#
# >> extract_nouns <<
# Input: a description string
# Output: all the nouns in the string
#
ozp_extract_nouns <- function(input.noun) {

    input.noun = input.noun
    output = POS_tag_documents(input.noun)

    output = as.data.frame(output$Document_1)

    output = filter(output, tags == "NN" | tags == "NNP" | tags =="NNS")

    return(output)
}

ozp_generate_keywords <- function(input) {

    output.ozp.generate.keywords <- list()

    # split the descriptions on ';'
    # input=test_input
    input.list = strsplit(tolower(input), ";")

    # unlist it because lists are hard to work with
    unlisted.input.list = unlist(input.list)

    # check if the input is not of length 1 to avoid problems with the POS tagging function

    # do a for loop over the unlisted input to generate  noun keywords
    #for (description in 1:length(unlisted.input.list)) {
        ## extract all nouns from the description

            #extracted.nouns.list = (ozp_generate_keywords_nouns_adjectives(unlisted.input.list[[description]],2))    
    #}
            ## the text tokens are always stored in the first list element so unlist that element
            #extracted.nouns.unlisted = unlist(extracted.nouns.list[[1]])

            ## use a for loop to add each noun to the output list
            #for (noun in 1:length(extracted.nouns.unlisted)) {
                ## make a char from the extracted keyword using paste
                #keyword = paste(extracted.nouns.unlisted[noun])
                ## stem the keyword
                #keyword = text_tokens(keyword, stemmer = "en")[[1]]

                ## add the noun to the list, duplicates are added aswell so we need to remove those later
                #output.ozp.generate.keywords = c(output.ozp.generate.keywords, keyword)
            #}
        # do a for loop over the unlisted input to generate noun keywords
        for (description in 1:length(unlisted.input.list)) {
            # extract all the noun adjectives from the description 
            extracted.nouns.list = (ozp_generate_keywords_nouns_adjectives(unlisted.input.list[[description]],2))
            # for each noun ajdective pair in the list extract the value and append it to the result list
            if (length(extracted.nouns.list) > 0) {
                for (noun.adj.pair in 1:length(extracted.nouns.list)) {
                    output.ozp.generate.keywords = c(output.ozp.generate.keywords, extracted.nouns.list[[noun.adj.pair]])
                }
            }

        }


        # do a for loop over the unlisted input to generate noun adjective keywords
        for (description in 1:length(unlisted.input.list)) {
            # extract all the noun adjectives from the description 
            extracted.adjective.nouns.list = (ozp_generate_keywords_nouns_adjectives(unlisted.input.list[[description]],1))
            # for each noun ajdective pair in the list extract the value and append it to the result list
            if (length(extracted.adjective.nouns.list) > 0) {
                for (noun.adj.pair in 1:length(extracted.adjective.nouns.list)) {
                    output.ozp.generate.keywords = c(output.ozp.generate.keywords, extracted.adjective.nouns.list[[noun.adj.pair]])
                }
            }

        }

        # remove all double values using dplyr distinct. Use a tempoary value to improve readability
        output.ozp.generate.keywords.distinct = distinct(as.data.frame(unlist(output.ozp.generate.keywords)))
        # get all the unique values from the distinct output.
        output.ozp.generate.keywords = paste(output.ozp.generate.keywords.distinct$`unlist(output.ozp.generate.keywords)`)

    return(output.ozp.generate.keywords)
    }
    

##test.input=longlist.input[4,2]
#test.output=ozp_generate_keywords(test.input)

#test.input = longlist.input[26, 2]
##input=test.input