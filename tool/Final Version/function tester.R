# set working directory to openmasses/tool/Final Version



# extract nouns tester

test_extract_nouns <- function() {
    # load the method
    source("functions/extract nouns.r")

    # input to test the method
    input = "the female executive ratio is defined by the ratio of female executives to male executive"

    output = ozp_extract_nouns(input)
    return(output)
}

test_extract_nouns_adjectives <- function() {

    # load the method
    source("functions/generate_keywords_noun_adjective.r")
    input = "the female executive ratio is defined by the ratio of female executives to male executives whereas the happy executives are excluded."

    output = ozp_generate_keywords_nouns_adjectives(input)
    return(output)
}


test = test_extract_nouns_adjectives()