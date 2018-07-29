# new document load stemming

# laad alle libraries in
require(tidyverse)
require(corpus)
require(tm)
require(pdftools)


# cas clean text function

cleanText <- function(pdfs.text) {

    # Replace line breaks, page breaks and tabs with a space
    pdfs.text <- lapply(pdfs.text, function(x) gsub("[\r\n\t]", " ", x))

    # Replace slashes
    pdfs.text <- lapply(pdfs.text, function(x) gsub("\u2215|\u2044", " ", x))

    # Replace parentheses with a space
    pdfs.text <- lapply(pdfs.text, function(x) gsub("\\(", " ", x))
    pdfs.text <- lapply(pdfs.text, function(x) gsub("\\)", " ", x))

    # Remove all full stops
    pdfs.text <- lapply(pdfs.text, function(x) gsub(".", "", x, fixed = TRUE))

    # Remove all question marks, quotation marks, asterisks, bullet points and weird symbols
    pdfs.text <- lapply(pdfs.text, function(x) gsub("?", "", x, fixed = TRUE))
    pdfs.text <- lapply(pdfs.text, function(x) gsub("\"", "", x, fixed = TRUE))
    pdfs.text <- lapply(pdfs.text, function(x) gsub("*", "", x, fixed = TRUE))
    pdfs.text <- lapply(pdfs.text, function(x) gsub("●", "", x, fixed = TRUE))
    pdfs.text <- lapply(pdfs.text, function(x) gsub("", "", x, fixed = TRUE))

    # Remove all sorts of commas, full stops, colons, semicolons, question marks, exclamation points, quotation marks, euro signs, dollar signs, pound signs
    pdfs.text <- lapply(pdfs.text, function(x) gsub("\u002C|\uFF0C|\u0326|\u02BB|\u003A|\uFF1A|\u003B|\uFF1B|\u204F|\uFE54|\u003F|\uFF1F|\u0021|\uFF01|\u201C|\u2018|\u2019|\u201D|\u20AC|\u0024|\uFF04|\u00A3|\uFFE1", "", x))

    # Change all text to lower case
    pdfs.text <- lapply(pdfs.text, tolower)

    return(pdfs.text)
}

clean_text <- function(text) {

    # Replace line breaks, page breaks and tabs with a space
    text <- lapply(text, function(x) gsub("[\r\n\t]", " ", x))

    # Replace slashes
    text <- lapply(text, function(x) gsub("\u2215|\u2044", " ", x))

    # Replace parentheses with a space
    text <- lapply(text, function(x) gsub("\\(", " ", x))
    text <- lapply(text, function(x) gsub("\\)", " ", x))

    # Remove all full stops
    text <- lapply(text, function(x) gsub(".", "", x, fixed = TRUE))

    # Remove all question marks, quotation marks, asterisks, bullet points and weird symbols
    text <- lapply(text, function(x) gsub("?", "", x, fixed = TRUE))
    text <- lapply(text, function(x) gsub("\"", "", x, fixed = TRUE))
    text <- lapply(text, function(x) gsub("*", "", x, fixed = TRUE))
    text <- lapply(text, function(x) gsub("?", "", x, fixed = TRUE))
    text <- lapply(text, function(x) gsub("?", "", x, fixed = TRUE))

    # Remove all sorts of commas, full stops, colons, semicolons, question marks, exclamation points, quotation marks, euro signs, dollar signs, pound signs
    text <- lapply(text, function(x) gsub("\u002C|\uFF0C|\u0326|\u02BB|\u003A|\uFF1A|\u003B|\uFF1B|\u204F|\uFE54|\u003F|\uFF1F|\u0021|\uFF01|\u201C|\u2018|\u2019|\u201D|\u20AC|\u0024|\uFF04|\u00A3|\uFFE1", "", x))

    # Change all text to lower case
    text <- lapply(text, tolower)

    return(text)
}


ozp_text_to_corpus <- function(text) {

   # text = "The emissions of CO2 or equivalent gasses made by the organisation. It is usually measured in tonnes."

    # clean the raw text: remove lowercase, punctuation and unwanted characters
    cleaned_description = clean_text(text)

    description_vector = VectorSource(cleaned_description)

    description_corpus = Corpus(description_vector)

    description_corpus <- tm_map(description_corpus, content_transformer(tolower))
    description_corpus <- tm_map(description_corpus, removePunctuation)
    description_corpus <- tm_map(description_corpus, stripWhitespace)
    description_corpus <- tm_map(description_corpus, removeWords, stopwords("english")) # remove stopwords
    description_corpus <- tm_map(description_corpus, stemDocument)


    #description_tdm = TermDocumentMatrix(description_corpus)

    return(description_corpus)
}

# laad een pdf in
pdf.text <- pdf_text("2016 Bath.pdf")
pdf.text <- cleanText(pdf.text)


#' Title
#'
#' @param input.pdf in list format
#'
#' @return a stemmed pdf in list format
#' @export
#'
#' @examples
ozp_pdf_stemming <- function(input.pdf) {
    # set to output object
    output.pdf = input.pdf
    #make a vectorsource for the input pdf
    output.pdf.vector.source = VectorSource(output.pdf)
    #make a corpus using the vector
    output.pdf.corpus = tm::Corpus(output.pdf.vector.source)

    # clean and stem the text using tm_map this removes whitespace punctuation and stopwords.
    output.pdf.corpus <- tm_map(output.pdf.corpus, content_transformer(tolower))
    output.pdf.corpus <- tm_map(output.pdf.corpus, removePunctuation)
    output.pdf.corpus <- tm_map(output.pdf.corpus, stripWhitespace)
    output.pdf.corpus <- tm_map(output.pdf.corpus, removeWords, stopwords("english")) # remove stopwords
    output.pdf.corpus <- tm_map(output.pdf.corpus, stemDocument)

    # convert the corpus back to a list.
    # this is done to avoid problems with the existing code. 
    # Use a true term document matrix in the future.
    output.text = get("content", output.pdf.corpus)

    return(output.text)

}

# get corpus
test.corpus = ozp_text_to_corpus(pdf.text)


test.corpus= desc(pdf.text)



test.tdm = description_to_tdm(pdf.text[[1]])


tdm.tfidf = weightTfIdf(test.tdm)

test.output.tfidf = tm_term_score(tdm.tfidf, c("sustainability", "research"))
test.output.frequency = tm_term_score(test.tdm, "sustain")

library(corpus)

term.stats=term_stats(test.tdm,ngrams = 2)


# 2-gram test

BigramTokenizer <-
    function(x)
        unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)

bigram.test.tdm <- TermDocumentMatrix(pdf.text, control = list(tokenize = BigramTokenizer))
inspect(removeSparseTerms(tdm[, 1:10], 0.7))