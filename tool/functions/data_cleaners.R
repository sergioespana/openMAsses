#
# >> form_term_table <<
# Description: Creates the table of search terms for visualization
# Input: search terms as vector
# Output: table with search terms per category
#

form_term_table <- function(terms) {
  table <- data.frame(matrix(c('hashtags','usernames','news articles', 'reddit','','','',''), ncol=2), stringsAsFactors = FALSE)
  
  terms <- strsplit(terms,';')
  
  hashtags <- ''
  news <- ''
  reddit <- ''
  usernames <- ''
  
  for (term in terms[[1]]) {
    
    term <- trimws(term)
    term <- gsub(',','',term)
    
    #The term cannot be empty
    if (term == ''){
      next
    }
    
    #If the user overrides the auto detector
    if (grepl('htg:', term) == TRUE || grepl('usn:', term) == TRUE || grepl('nws:', term) == TRUE || grepl('rdt:', term) == TRUE) {
      
      if (grepl('htg:', term) == TRUE) {
        term <- gsub('htg:','', term)
        term <- trimws(term)
        hashtags <- paste(hashtags, term)
        hashtags <- paste(hashtags, ',', sep='')
        table[1,2] <- hashtags
      }
      else if (grepl('usn:', term) == TRUE) {
        term <- gsub('usn:','', term)
        term <- trimws(term)
        usernames <- paste(usernames,term)
        usernames <- paste(usernames, ',', sep='')
        table[2,2] <- usernames
      }
      else if (grepl('nws:', term) == TRUE) {
        term <- gsub('nws:','', term)
        term <- trimws(term)
        news <- paste(news, term)
        news <- paste(news, ',', sep='')
        table[3,2] <- news
      }
      else if (grepl('rdt:', term) == TRUE) {
        term <- gsub('rdt:','', term)
        term <- trimws(term)
        reddit <- paste(reddit, term)
        reddit <- paste(reddit, ',', sep='')
        table[4,2] <- reddit
      }
    }
    #otherwise, the autodetector will determine the proper group for the term
    else if (grepl('#', term) == TRUE && grepl(' ', term) == FALSE) {
      hashtags <- paste(hashtags, term)
      hashtags <- paste(hashtags, ',', sep='')
      table[1,2] <- hashtags
    }
    
    else if (grepl(' ', term) == FALSE && grepl('#', term) == FALSE) {
      usernames <- paste(usernames,term)
      usernames <- paste(usernames, ',', sep='')
      table[2,2] <- usernames
    }
    
    else if (grepl(' ', term) == TRUE && grepl('#', term) == FALSE) {
      news <- paste(news, term)
      news <- paste(news, ',', sep='')
      table[3,2] <- news
    }
    
    else if (grepl(' ', term) == TRUE && grepl('#', term) == FALSE) {
      reddit <- paste(reddit, term)
      reddit <- paste(reddit, ',', sep='')
      table[4,2] <- reddit
    }
  }

  return (table)
}


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