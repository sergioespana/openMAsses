# 
# >> prepareWordCloudPDF <<
# Description: Creates a word frequency list from the imported pdfs
# Input: text from pdf(s)
# Output: word frequency list
#  

source("functions/data_loaders.R")

prepareWordCloudPDF <- function(pdfs.text) {
  
  withProgress(message = 'Generating Word Cloud', value = 0, {
    
    # Create corpus
    corpus <- Corpus(VectorSource(pdfs.text))
    incProgress(1 / 2)
    
    # Create TermDocumentMatrix
    tdm <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE,
                                                     stopwords = TRUE,
                                                     tolower = TRUE,
                                                     stemming = FALSE,
                                                     removeNumbers = TRUE,
                                                     bounds = list(global = c(1, Inf))))
    
    incProgress(1 / 2)
    
    # Create the table with the correct names
    result <- as.matrix(tdm)
    result <- rowSums(result)
  })
  return(result)
}

#
# >> PrepareWordCloudMedia <<
# Creates a term frequency list from the imported python output
# Input: Plain text file, !should be updated into a combined media file later!
# output: term frequency list
#

prepareWordCloudMedia <- function(media_text) {
  
  withProgress(message = 'Generating Word Cloud', value = 0, {
    
    # Create corpus
    corpus <- Corpus(VectorSource(media_text))
    
    # Create TermDocumentMatrix
    tdm <- TermDocumentMatrix(corpus, control = list(bounds = list(global = c(1, Inf))))
    incProgress(1 / 2)
    
    # Create the table with the correct names
    result <- as.matrix(tdm)
    result <- rowSums(result)
  })
  return(result)
}

# 
# >> prepareWordCloudLonglist << BROKEN
# Description: Creates a term frequency list from a tdm? This seems broken too
# Input: term document matrix
# Output: term frequency list
# 

prepareWordCloudLonglist <- function(tdm) {
  frequency <- c(tdm[, 2])
  synonym = list()
  
  for (i in 1:nrow(tdm)) {
    split <- strsplit(toString(tdm[i, 1]), " / ")
    split <- split[[1]][1]
    synonym <- c(synonym, split)
  }
  
  # only take the leaf element to display in the word cloud
  for (element in 1:length(synonym)) {
    value=synonym[[element]]
    synonym[[element]] = sapply(strsplit(value, ";", fixed = TRUE), tail, 1)
  }
  
  names(frequency) <- synonym
  print(frequency)
  return(frequency)
}

# 
# >> generateWordCloud <<
# Description: Creates a wordcloud based on the word frequency list and the number of terms selected 
# by the slider in ui.R
# Input: term / word frequency list and number of term / words to display
# Output: word cloud
#

generateWordCloud <- function(frequency, number) {
  
  # Check if the number of terms to show in the word cloud is not bigger than the number of words to show
  if (number > length(frequency)) {
    number <- length(frequency)
  }
  
  # Sort the frequencies
  frequency <- sort(frequency, decreasing = TRUE)
  
  words <- names(frequency)
  return(wordcloud(words[1:number], frequency[1:number], min.freq = 1, random.order = FALSE, rot.per = 0.35, use.r.layout = FALSE, colors = brewer.pal(4, "Paired")))
}
