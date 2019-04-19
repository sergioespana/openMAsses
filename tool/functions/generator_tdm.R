#
# >> Create_TDM <<
# Description: Creates the document term matrix based on the input
# Input: text from pdf(s), longlist terms, scoring scheme and threshold
# Output: term document matrix
#

create_TDM <- function(inputText, longlist, scheme, threshold, longlistMode) {
  #Old function, heavy and slow, but left there because I didnt bother optimizing it
  if (longlistMode == 1) {
    tdm <- create_TDMOld(inputText, longlist, scheme, threshold, longlistMode)
    return(tdm)
  }
  else if (longlistMode == 2)
  {
    #Clean longlist to get proper working material
    longlist <- apply(longlist, c(1,2), function(x) tolower(x))
    longlist[,'Descriptions'] <- sapply(longlist[,'Descriptions'], function(x) stemDocument(x))
    
    #Split all the descriptions into a single vector for tagging
    allDescriptionsList <- strsplit(longlist[,'Descriptions'], ';',  fixed = TRUE)
    allDescriptions <- unlist(allDescriptionsList)
    
    #Cycle through each text, tag all the words you can find
    tdm <- matrix(nrow = length(longlist[,'Category']), ncol = length(inputText))
    tdm <- rbind(sapply(inputText, function(x) count_terms(x, allDescriptions, allDescriptionsList)))
    #rownames(tdm) <- longlist[,'Category']
    
    tdm <- data.frame(tdm)
    
    #add score and categories to table
    tdm$Score <- add_scoreNew(tdm,scheme)
    
    
    tdm$Category <- longlist[,'Category']
    
    #Reshuffle for readability of table
    tdmFormatted <- cbind(tdm$Category,tdm$Score,tdm[,c(1:length(inputText))])
    colnames(tdmFormatted)[c(1,2)] <- c('Category', 'Score')
  }
  return(tdmFormatted)
}

#
# >> count_terms <<
# Description: Counts the number of terms in the text for a topic
# Input: textfile with words, longlist descriptions
# Output: matrix with terms count for a topic
# 

count_terms <- function (inputText, descriptions, descriptionsList) {
  hitsVector <- str_count(inputText, descriptions)
  
  topicVector <- c()
  newstart <- 0
  for (i in 1:length(descriptionsList)) {
    lengthTopic <- length(descriptionsList[[i]])
    topicVector <- append(topicVector, sum(hitsVector[1+newstart:lengthTopic]))
    newstart <- newstart + lengthTopic
  }
  return (topicVector)
}

#
# >> add_scoreNew <<
# Description: Based on the scoring scheme, returns a vector with the scores
# Input: tdm, scoring scheme
# Output: Vector with scores
# 

add_scoreNew <- function(tdm, scheme) {

  if (scheme == 1) {
    tdm[tdm > 1] <- 1
    ScoreVector <- rowSums(tdm)
  }
  else if (scheme == 2) {
    ScoreVector <- rowSums(tdm) 
  }
  else if (scheme == 3) {
    #Relative, check for implementaiton later
    return(rep(0,nrow(tdm)))
  }
  else if (scheme == 4) {
    #Look into this scheme later as well, it is odd
    freq <- rowSums(tdm)
    tdm[tdm > 1] <- 1
    occurance <- rowSums(tdm)
    occurance[occurance < 1] <- 1
    ScoreVector <- freq/occurance/ncol(tdm)
  }
  return (ScoreVector)
}

# 
# >> createTDM <<
# Input: text from pdf(s), longlist terms, scoring scheme and threshold
# Output: term document matrix
# 

# REQUIRES SERIOUS REFACTORING AND OPTIMIZAITON

create_TDMOld <- function(input.text, longlist, scheme, threshold, longlist.mode) {
  withProgress(message = 'Generating Table', value = 0, {
    #longlist.mode #what is this supposed to do?
    print(input.text)
    # set topic and description column number.
    # this was done to increase the readability of the code
    #wait, what readibility?
    topic_col_nr = 1
    description_col_nr = 2
    columns <- NULL
    
    # test optimalization effort
    optimized_descriptions = list()
    
    for (row in 1:nrow(longlist)) { #Seriously, this forloop needs refactoring, look at this atrocity
      if (longlist.mode == 1) {
        #current_topic_name_unfiltered = toString(longlist[row, language.column])
        
        ## get the current topic
        #current_descriptionsterms <- toString(str_extract(toString(longlist[row, description_col_nr]), "[^;]*$"))
        
        # New extraction

        
        extracted.description <- longlist[[row, description_col_nr]]
        if (extracted.description != "") {
          # Extract the synonyms per row
          # old term list generation
          # terms.list <- strsplit(tolower(terms), ";")
          # extracted.terms.list <- strsplit(tolower(terms))
          
          # debug statement
          test_input = longlist[row, description_col_nr]
          
          # new term list generation
          terms.list <- ozp_generate_keywords(longlist[row, description_col_nr])
          if (length(optimized_descriptions) == 0) {
            optimized_descriptions = list(terms.list)
          } 
          else {
            optimized_descriptions = c(optimized_descriptions, list(terms.list))
          }
        }
        else {
          if (length(optimized_descriptions) == 0) {
            optimized_descriptions = list("") 
          } 
          else {
            optimized_descriptions = c(optimized_descriptions, "")
          }
        }
      } 
      else {
        extracted.description <- longlist[[row, description_col_nr]]
        extracted.description <- tolower(extracted.description)
        
        if (extracted.description != "") {
          # Extract the synonyms per row
          # old term list generation
          splitted.descriptions = str_split(extracted.description, ';')
          
          # new term list generation
          terms.list <- splitted.descriptions[[1]]
          stemmed.list <- list()
          # apply stemming to each keyword in the list
          for (keyword in 1:length(terms.list)) {
            stemmed.keyword = paste(SnowballC::wordStem(terms.list, language = "english"))
            stemmed.list = c(stemmed.list, stemmed.keyword)
          }
          
          #for (keyword in 1:length(splitted.string.list)) {
          #stemmed.keyword = paste(SnowballC::wordStem(splitted.string.list[[keyword]], language = "english"))
          #stemmed.list=c(stemmed.list,stemmed.keyword)
          
          ##stemmed.list=c(stemmed.list,paste(SnowballC::wordStem(splitted.string.list[[keyword]],language = "english")))
          #}
          
          terms.list = stemmed.list
          if (length(optimized_descriptions) == 0) {
            optimized_descriptions = list(terms.list)
          } 
          else {
            optimized_descriptions = c(optimized_descriptions, list(terms.list))
          }
        }
        else {
          if (length(optimized_descriptions) == 0) {
            optimized_descriptions = list("")
          } 
          else {
            optimized_descriptions = c(optimized_descriptions, "")
          }
        }
      }
    }

    # Iterate over all categories
    for (cat in 1:length(names(input.text))) {
      
      catName <- names(input.text)[cat]
      
      # If it is a category with PDFs underneath it add [Category] to the name
      if (!grepl('.pdf', catName, fixed = TRUE)) {
        catName <- paste("[Category]", catName, sep = " ")
      }
      
      # Remove '.pdf', dashes and hyphens from pdf name
      catName <- gsub(".pdf", "", catName)
      catName <- gsub("_", " ", catName)
      catName <- gsub("-", " ", catName)
      columns[cat] <- catName
    }
    
    # Create empty TermDocumentMatrix
    tdm <- data.frame(matrix(ncol = length(columns) + 1, nrow = 0), stringsAsFactors = FALSE)
    colnames(tdm) <- c("Longlist", columns)
    
    # Store the longlist topics in the first column
    for (row in 1:nrow(longlist)) {
      terms <- toString(longlist[row, topic_col_nr])
      #terms <- toString(str_extract(toString(longlist[row, topic_col_nr]), "[^;]*$"))
      tdm[row, 1] <- terms
    }

    
    # Create an empty log file

    #sink("outfile.txt") #Crashes the file


    # Iterate over every category 
    for (l in 1:length(columns)) {
      
      # introduce loop here
      pdfs.text <- input.text[[l]]
      categoryName <- columns[l]
      incProgress(1 / length(columns), detail = paste("Scanning: ", categoryName))
      
      # Create category table
      pdfNames <- names(pdfs.text)
      catTDM <- data.frame(matrix(ncol = length(pdfNames) + 1, nrow = 0), stringsAsFactors = FALSE)
      colnames(catTDM) <- c("Longlist", pdfNames)
      
      # Iterate over every pdf in the category
      for (pdf in 1:length(pdfs.text)) {
        pdfName <- pdfNames[pdf]
        pdfPages <- pdfs.text[[pdf]]
        
        ## Determine langauge for the pdf by scanning the first 10 words of each page.
        languages <- textcat(pdfPages[1:10])
        language <- names(which.max(table(languages)))
        #language.column <- grep(language, colnames(longlist), ignore.case=TRUE)
        
        ## If the detected language is not found in the longlist it will use the first row
        #if(length(language.column) == 0){
        language.column <- topic_col_nr
        #}

        
        # Iterate over every row in the longlist
        for (row in 1:nrow(longlist)) {
          
          #current_topic_name_unfiltered = toString(longlist[row, language.column])
          
          ## get the current topic
          #current_descriptionsterms <- toString(str_extract(toString(longlist[row, description_col_nr]), "[^;]*$"))
          
          # New extraction
          
          extracted.description <- longlist[[row, description_col_nr]]
          #ozp_generate_keywords(longlist[row,description_col_nr])
          
          
          # Extract the row

          
          # hier worden momenteel de descriptions geretrieved.
          # terms <- toString(str_extract(toString(longlist[row, description_col_nr]), "[^;]*$"))
          terms.frequency <- 0
          
          # Check if row is not empty
          if (extracted.description != "") {
            # Extract the synonyms per row
            # old term list generation
            # terms.list <- strsplit(tolower(terms), ";")
            # extracted.terms.list <- strsplit(tolower(terms))
            
            
            # debug statement
            test_input = longlist[row, description_col_nr]
            
            
            # new term list generation
            #terms.list <- ozp_generate_keywords(longlist[row, description_col_nr])
            
            # optimization test
            terms.list <- optimized_descriptions[[row]]
            
            # terms.list <- ozp_generate_keywords(longlist[row, description_col_nr])
            # terms.list <- ozp_generate_keywords("this is a description")
            # Iterate over the synonyms

            if (length(terms.list) > 0) {
              for (synonym in 1:length(terms.list)) {
                
                # Get the frequency of the synonym
                term <- terms.list[[synonym]]
                term.frequency <- get_frequency(term, pdfPages, categoryName, pdfName, language)
                terms.frequency <- terms.frequency + term.frequency
              }
            }
          }
          
          # If the scoring scheme is 'relative', we use the frequency divided by pages
          if (scheme == 3) {
            npages <- length(pdfPages)
            score <- terms.frequency / npages
            score <- round(score, digits = 3)
            catTDM[row, pdf + 1] <- score
          }
          
          # If the scoring scheme is not 'relative', we can use the frequency
          else {
            
            # Save frequency of synonyms to dataframe
            catTDM[row, pdf + 1] <- terms.frequency
          }
        }
      }
      
      # After a category table is created (containing all the pdfs of that category), add the score column to it
      catTDM <- add_score(catTDM, scheme, threshold, TRUE)
      
      # Add the score column of the category table to the main table
      tdm[1:nrow(tdm), l + 1] <- catTDM[1:nrow(catTDM), 2]
    }
    
    # After all the categories columns are added, add the score column to the main tdm
    tdm <- add_score(tdm, scheme, threshold, FALSE)
    
    # Set the correct names again
    colnames(tdm) <- c("Longlist", "Score", columns)
    
  })
  print(tdm)
  return(tdm)
}




#
# >> get_frequency <<
# Input: term, pdf text, category name, pdf name, language
# Output: the number of times the term occures in the pdf 
#

get_frequency <- function(term, pdf, categoryName, pdfName, language){
  frequency <- 0
  
  # Add a space before and after the term to avoid counting terms that are part of a bigger term
  term <- paste(" ", term, " ", sep="")
  
  # Iterate over the paragraphs
  for(page in 1:length(pdf)){
    frequency.page <- str_count(pdf[page], term)
    frequency <- frequency + frequency.page
    if(frequency.page > 0){
      
      # In case a term is found on a pdf page, print the pdf name, the language, the term, the page number and the number of occurences to the log file
      # If pdf is in a category, also print the category name to the log
      if (grepl("[Category]", categoryName, fixed=TRUE)){
        categoryName <- gsub('[Category] ','[Category]', categoryName, fixed = TRUE)
        log <- paste(categoryName, " [PDF]", pdfName, "  [LANGUAGE]", language, "  [TERM]", term, "  [PAGE]", page, "  [OCCURRENCES]", frequency.page, "\n", sep = "")
      }
      else{
        log <- paste("[PDF]", pdfName, "  [LANGUAGE]", language, "  [TERM]", term, "  [PAGE]", page, "  [OCCURRENCES]", frequency.page, "\n", sep = "")
      }
      
      # Log every match that is found in the PDFs 
      cat(log)
    }
  }
  
  return(frequency)
}

# 
# >> addScore <<
# Input: term document matrix, scoring scheme, threshold and a boolean whether it is a category tdm or the main tdm
# Output: term document matrix with a score column
# 

add_score <- function(tdm, scheme, threshold, category) {
  
  print(tdm)
  
  # If the scoring scheme is 'count'
  if (scheme == 1) {
    
    # If the tdm has only one category/pdf
    if (ncol(tdm) == 2) {
      
      # Add a score column with a total of all the columns
      tdm <- as.data.frame(append(tdm, list(Score = tdm[, 2]), after = 1))
      
      # Iterate over each topic in the longlist
      for (i in 1:nrow(tdm)) {
        
        # If the topic occurs in at least ones, change it to 1 (so 0 stays 0)
        if (tdm[i, 2] > 0) {
          tdm[i, 2] <- 1
        }
      }
    }
    
    # If the tdm has more than one category/pdf
    else {
      
      # Add a score column with 1 if at least one other column is bigger than 0
      tdm <- as.data.frame(append(tdm, list(Score = rowSums(tdm[, 2:ncol(tdm)] > 0)), after = 1))
    }
  }
  
  # If the scoring scheme is 'frequency' or if the scoring scheme is 'weighted' & it is a category table.
  # We do not want to lower the frequency twice (we do this only for the main table)
  else if (scheme == 2 || (scheme == 4 && category)) {
    
    # If the tdm has only one category/pdf
    if (ncol(tdm) == 2) {
      
      # Add a score column that is equal to the one column
      tdm <- as.data.frame(append(tdm, list(Score = tdm[, 2]), after = 1))
    }
    
    # If the tdm has more than one category/pdf
    else {
      
      # Add a score column with a total of all the columns
      tdm <- as.data.frame(append(tdm, list(Score = rowSums(tdm[, 2:ncol(tdm)])), after = 1))
    }
  }
  
  # If the scoring scheme is 'relative'
  else if (scheme == 3) {
    
    # Add a score column with a total of all the columns
    tdm <- as.data.frame(append(tdm, list(Score = tdm[, 2]), after = 1))
    
    # Iterate over each row in the matrix
    for (row in 1:nrow(tdm)) {
      score <- 0
      
      # Iterate over each pdf in the matrix
      for (pdf in 3:ncol(tdm)) {
        
        value <- tdm[row, pdf]
        
        # If it is a category table
        if (category) {
          
          # Count 1 if value is bigger or equal to the threshold
          if (value >= threshold) {
            score <- score + 1
          }
        }
        
        # If it is the main table
        else {
          
          # Count 1 if value is bigger than 0
          # Because for each category we already did the bigger or equal to the threshold check
          if (value > 0) {
            score <- score + 1
          }
        }
      }
      
      # Save the score to the Score column
      tdm[row, 2] <- score
    }
  }
  
  # If the scoring scheme is 'weighted'
  else {
    
    # If the tdm has only one category/pdf
    if (ncol(tdm) == 2) {
      
      # Add a score column that is equal to the one column
      tdm <- as.data.frame(append(tdm, list(Score = tdm[, 2]), after = 1))
    }
    
    # If the tdm has more than one category/pdf
    else {
      
      # Get the total frequency for each longlist topic
      tdm.frequency <- list(rowSums(tdm[, 2:ncol(tdm)]))
      
      # Get the number of categories/pdfs for each longlist topic for which the frequency is bigger than 0
      tdm.count <- list(rowSums(tdm[, 2:ncol(tdm)] > 0))
      
      # Divide the total frequency for each longlist topic by the total number of categories/pdfs
      tdm.mixed <- mapply('/', tdm.frequency, (ncol(tdm) - 1))
      
      # Multiply the above by the number of categories/pdfs with a score higher than 0
      tdm.mixed <- list(Score = unlist(tdm.mixed) * unlist(tdm.count))
      
      # Add the score column to the tdm
      tdm <- as.data.frame(append(tdm, tdm.mixed, after = 1))
    }
  }

  return(tdm)
}