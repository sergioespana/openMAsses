library(tm)

description_to_tdm <- function(text) {

    text = "The emissions of CO2 or equivalent gasses made by the organisation. It is usually measured in tonnes."

    # clean the raw text: remove lowercase, punctuation and unwanted characters
    cleaned_description = clean_text(text)

    description_vector = VectorSource(cleaned_description)

    description_corpus = Corpus(description_vector)


    description_corpus <- tm_map(description_corpus, content_transformer(tolower))
    description_corpus <- tm_map(description_corpus, removePunctuation)
    description_corpus <- tm_map(description_corpus, stripWhitespace)
    description_corpus <- tm_map(description_corpus, removeWords, stopwords("english")) # remove stopwords
    description_corpus <- tm_map(description_corpus, stemDocument)


    description_tdm = TermDocumentMatrix(description_corpus)
}