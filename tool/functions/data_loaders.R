#' This script contains all functions that either load, preprocess or clean text.

# 
# >> DocumentsLoad <<
# Description: unzipping files and loading the text from the pdf file(s)
# Input: folder path
# Output: list with all the text in the pdf(s) per category
# 

documentsLoad <- function(files) {
  withProgress(message = 'Reading documents', value = 0, {
    
    pdfs <- c() # create vector
    categories <- c() #create vector
    
    # Iterate over all the uploaded files
    number.files <- length(files[, 1])
    for (i in 1:number.files) {
      fileName <- files[[i, 'name']]
      incProgress(0, detail = "Scanning documents")
      
      # If the file is a .zip, unzip it
      if (grepl(".zip", fileName, fixed = TRUE)) {
        
        zipFiles <- unzip(files[[i, 'datapath']], list = TRUE)
        
        
        # Iterate over each file in the zip
        for (n in 1:nrow(zipFiles)) {
          zipContent <- zipFiles[n, "Name"]
          #observe(print(zipContent))
          zipName <- gsub('.zip', '', fileName)
          mainFolder <- paste(zipName, "/", sep = "")
          
          # Only check the file in the zip if it is valid
          if (str_count(zipContent, "__MACOSX") == 0 && str_count(zipContent, ".DS_Store") == 0 && mainFolder != zipContent) {
            
            # If the file is a pdf, read the pdf #bad comment
            if (grepl(".pdf", zipContent)) {
              if (str_count(zipContent, "/") == 1) {
                # possible category stuff # oftewel er is een folder structure aanwezig # >=1
                categories[length(categories) + 1] <- zipContent # insert value into vector.
              }
              pdfs[length(pdfs) + 1] <- zipContent
            }
            # If the file within the zip is an invalid format, ignore it
            else if (grepl(".xls", zipContent, fixed = TRUE) || grepl(".xlsx", zipContent, fixed = TRUE) || grepl(".doc", zipContent, fixed = TRUE) || grepl(".docx", zipContent, fixed = TRUE) || grepl(".jpg", zipContent, fixed = TRUE) || grepl(".png", zipContent, fixed = TRUE) || grepl(".iso", zipContent, fixed = TRUE) || grepl(".txt", zipContent, fixed = TRUE)) {
              # Do nothing
            }
            # If the file is a folder, add as category
            else {
              zipContent <- gsub('/', '', gsub(zipName, '', zipContent))
              categories[length(categories) + 1] <- zipContent
            }
          }
        }
        unzip(files[[i, 'datapath']], exdir = "unzip", overwrite = TRUE)
      }
      # If the file is a .pdf add it as an own category and read the pdf
      else if (grepl(".pdf", fileName)) {
        categories[length(categories) + 1] <- paste(fileName)
        pdfs[length(pdfs) + 1] <- paste(fileName)
      }
      # Documents other than pdf or zip are ignored
    }
    
    allpdfs.text = list()
    
    #observe(print(typeof(categories)))
    
    # Iterate over each category
    for (number in 1:length(categories)) {
      category <- categories[number]
      pdfs.text <- NULL
      
      incProgress(1 / length(categories), detail = category)
      # If it is a pdf that is directly uploaded
      if (str_count(category, "/") == 0 && str_count(category, ".pdf") == 1) {
        path <- files[[which(grepl(category, files$name, fixed = TRUE)), 'datapath']]
        
        # stem the PDF
        pdfs.text[[category]] <- ozp_pdf_stemming(pdf_text(path))
        
        # Clean text
        # Save pdf text as category
        allpdfs.text[[category]] <- pdfs.text
      }
      # If it is a pdf that is uploaded in a zip
      else if (str_count(category, "/") == 1) {
        
        # update this stuff.
        path <- paste("unzip/", category, sep = "")
        pos = regexpr('/', category)
        category <- substr(category, pos + 1, nchar(category))
        pdfs.text[[category]] <- ozp_pdf_stemming(pdf_text(path))
        
        # Clean text
        pdfs.text <- cleanText(pdfs.text)
        
        # New stemming here:
        pdfs.text <- ozp_pdf_stemming(pdfs.text)
        # Save pdf text as category
        allpdfs.text[[category]] <- pdfs.text
      }
      # If it is a category with pdfs underneath it
      else {
        positions <- which(grepl(paste("/", category, "/", sep = ""), pdfs, fixed = TRUE))
        
        # Iterate over each pdf file of this category
        for (k in 1:length(positions)) {
          category.pdf <- pdfs[positions[k]]
          path <- paste("unzip/", category.pdf, sep = "")
          pos <- gregexpr('/', category.pdf)
          pos <- pos[[1]][length(pos[[1]])]
          category.pdf <- substr(category.pdf, pos + 1, nchar(category.pdf))
          pdfs.text[[category.pdf]] <- pdf_text(path)
        }
        
        # Clean text old version
        pdfs.text <- cleanText(pdfs.text)
        # stemming
        pdfs.text <- ozp_pdf_stemming(pdfs.text)
        # Save text of all pdfs to category
        allpdfs.text[[category]] <- pdfs.text
      }
    }
  })
  return(allpdfs.text)
}

#
# >> LoadMedia <<
# Description: loads the articles from the python output and cleans them
# Input: datapath to the python output files
# Output: text documents from this output
#

loadMedia <- function(articles) {
  withProgress(message = 'loading news', value = 0, {
    
    print('load media test')
    articles_text <- read_lines(articles$datapath)
    articles_clean <- articles_text
    
    articles_clean <- gsub('\'','',articles_clean)
    articles_clean <- gsub('[','',articles_clean, fixed = TRUE)
    articles_clean <- gsub(']','',articles_clean, fixed = TRUE)
    articles_clean <- gsub('\"','',articles_clean, fixed = TRUE)
  })
  return(articles_clean)
}

# 
# >> LonglistLoad <<
# Description: Loading the longlist from the excel sheet(s)
# Input: datapath to file
# Output: merged longlist with a languages per column
# 

longlistLoad <- function(files){
  withProgress(message = 'Reading longlist files', value = 0, {
    
    number.files <- length(files[,1])
    
    # Merge longlist files to one list
    longlist.data <- NULL
    for(i in 1:number.files){
      incProgress(1/number.files, detail = paste(files[[i, 'name']]))
      file <- paste(files[[i, 'datapath']], sep = ".", "xlsx")
      longlist <- rbind(longlist.data, ozp_parse_longlist(file))
    }
  })
  return(longlist)
}

#
# >>DocumentsLoadWordCloud
# Description: Gets data from the pdfs, then transforms them into word cloud useful data
# Input: folder path to documents
# Output: list with all text in the pdfs per category
#
# Note: This function should only be used with the word clouds
#

documentsLoadwordcloud <- function(files) {
  withProgress(message = 'Reading documents', value = 0, {
    
    print('function documentsloadwordcloud is called')
    pdfs <- c()
    categories <- c()
    
    # Iterate over all the uploaded files
    number.files <- length(files[, 1])
    for (i in 1:number.files) {
      fileName <- files[[i, 'name']]
      incProgress(0, detail = "Scanning documents")
      
      # If the file is a .zip, unzip it
      if (grepl(".zip", fileName, fixed = TRUE)) {
        zipFiles <- unzip(files[[i, 'datapath']], list = TRUE)
        
        # Iterate over each file in the zip
        for (n in 1:nrow(zipFiles)) {
          zipContent <- zipFiles[n, "Name"]
          zipName <- gsub('.zip', '', fileName)
          mainFolder <- paste(zipName, "/", sep = "")
          
          # Only check the file in the zip if it is valid
          if (str_count(zipContent, "__MACOSX") == 0 && str_count(zipContent, ".DS_Store") == 0 && mainFolder != zipContent) {
            
            # If the file is a pdf, read the pdf
            if (grepl(".pdf", zipContent)) {
              if (str_count(zipContent, "/") == 1) {
                categories[length(categories) + 1] <- zipContent
              }
              pdfs[length(pdfs) + 1] <- zipContent
            }
            # If the file within the zip is an invalid format, ignore it
            else if (grepl(".xls", zipContent, fixed = TRUE) || grepl(".xlsx", zipContent, fixed = TRUE) || grepl(".doc", zipContent, fixed = TRUE) || grepl(".docx", zipContent, fixed = TRUE) || grepl(".jpg", zipContent, fixed = TRUE) || grepl(".png", zipContent, fixed = TRUE) || grepl(".iso", zipContent, fixed = TRUE) || grepl(".txt", zipContent, fixed = TRUE)) {
              # Do nothing
            }
            # If the file is a folder, add as category
            else {
              zipContent <- gsub('/', '', gsub(zipName, '', zipContent))
              categories[length(categories) + 1] <- zipContent
            }
          }
        }
        unzip(files[[i, 'datapath']], exdir = "unzip", overwrite = TRUE)
      }
      # If the file is a .pdf add it as an own category and read the pdf
      else if (grepl(".pdf", fileName)) {
        categories[length(categories) + 1] <- paste(fileName)
        pdfs[length(pdfs) + 1] <- paste(fileName)
      }
      # Documents other than pdf or zip are ignored
    }
    
    allpdfs.text = NULL
    
    # Iterate over each category
    for (number in 1:length(categories)) {
      category <- categories[number]
      pdfs.text <- NULL
      
      incProgress(1 / length(categories), detail = category)
      
      # If it is a pdf that is directly uploaded
      if (str_count(category, "/") == 0 && str_count(category, ".pdf") == 1) {
        path <- files[[which(grepl(category, files$name, fixed = TRUE)), 'datapath']]
        pdfs.text[[category]] <- pdf_text(path)
        
        # Clean text
        pdfs.text <- cleanText(pdfs.text)
        
        # Save pdf text as category
        allpdfs.text[[category]] <- pdfs.text
      }
      # If it is a pdf that is uploaded in a zip
      else if (str_count(category, "/") == 1) {
        path <- paste("unzip/", category, sep = "")
        pos = regexpr('/', category)
        category <- substr(category, pos + 1, nchar(category))
        pdfs.text[[category]] <- pdf_text(path)
        
        # Clean text
        pdfs.text <- cleanText(pdfs.text)
        
        # Save pdf text as category
        allpdfs.text[[category]] <- pdfs.text
      }
      # If it is a category with pdfs underneath it
      else {
        positions <- which(grepl(paste("/", category, "/", sep = ""), pdfs, fixed = TRUE))
        
        # Iterate over each pdf file of this category
        for (k in 1:length(positions)) {
          category.pdf <- pdfs[positions[k]]
          path <- paste("unzip/", category.pdf, sep = "")
          pos <- gregexpr('/', category.pdf)
          pos <- pos[[1]][length(pos[[1]])]
          category.pdf <- substr(category.pdf, pos + 1, nchar(category.pdf))
          pdfs.text[[category.pdf]] <- pdf_text(path)
        }
        
        # Clean text
        pdfs.text <- cleanText(pdfs.text)
        
        # Save text of all pdfs to category
        allpdfs.text[[category]] <- pdfs.text
      }
    }
  })
  return(allpdfs.text)
}

#
# >> cleanText <<
# Input: pdf text
# Output: cleaned pdf text
#

cleanText <- function(pdfs.text){
  
  print('function cleantext is called')
  
  # Replace line breaks, page breaks and tabs with a space
  pdfs.text <- lapply(pdfs.text, function(x)gsub("[\r\n\t]"," ",x))
  
  # Replace slashes
  pdfs.text <- lapply(pdfs.text, function(x)gsub("\u2215|\u2044"," ",x))
  
  # Replace parentheses with a space
  pdfs.text <- lapply(pdfs.text, function(x)gsub("\\("," ",x))
  pdfs.text <- lapply(pdfs.text, function(x)gsub("\\)"," ",x))
  
  # Remove all full stops
  pdfs.text <- lapply(pdfs.text, function(x)gsub(".","",x,fixed = TRUE))
  
  # Remove all question marks, quotation marks, asterisks, bullet points and weird symbols
  pdfs.text <- lapply(pdfs.text, function(x)gsub("?","",x,fixed = TRUE))
  pdfs.text <- lapply(pdfs.text, function(x)gsub("\"","",x,fixed = TRUE))
  pdfs.text <- lapply(pdfs.text, function(x)gsub("*","",x,fixed = TRUE))
  pdfs.text <- lapply(pdfs.text, function(x)gsub("●","",x,fixed = TRUE))
  pdfs.text <- lapply(pdfs.text, function(x)gsub("","",x,fixed = TRUE))
  
  # Remove all sorts of commas, full stops, colons, semicolons, question marks, exclamation points, quotation marks, euro signs, dollar signs, pound signs
  pdfs.text <- lapply(pdfs.text, function(x)gsub("\u002C|\uFF0C|\u0326|\u02BB|\u003A|\uFF1A|\u003B|\uFF1B|\u204F|\uFE54|\u003F|\uFF1F|\u0021|\uFF01|\u201C|\u2018|\u2019|\u201D|\u20AC|\u0024|\uFF04|\u00A3|\uFFE1","",x))
  
  # Change all text to lower case
  pdfs.text <- lapply(pdfs.text, tolower)
  
  return(pdfs.text)
}

#
# >> ozp_pdf_stemming <<
# description: I guess it stems all the words from the pdfs
# input: The text from pdfs
# output: The stems of the text from the pdfs
#

ozp_pdf_stemming <- function(input.pdf) {
  
  print('is this even used?')
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
