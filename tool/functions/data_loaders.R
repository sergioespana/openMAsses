#' This script contains all functions that either load, preprocess or clean text.
source("functions/data_cleaners.R")

# 
# >> DocumentsLoad <<
# Description: unzipping files and loading the text from the pdf and txt file(s)
# Input: folder path, whether stemming is needed
# Output: list with all the text in the pdf(s) per category
# 

load_documents <- function(files, stemming) {
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
      # If the file is a .pdf or .txt (for media) add it as an own category and read the pdf
      else if (grepl(".pdf", fileName) || grepl('.txt', fileName)) {
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
      # If it is a pdf or text file, the text is extracted
      if (str_count(category, "/") == 0 && (str_count(category, ".pdf") == 1) || str_count(category, '.txt') == 1) {
        path <- files[[which(grepl(category, files$name, fixed = TRUE)), 'datapath']]
        
        #If the file is a pdf or textfile, load the text
        if (str_count(category, '.pdf') == 1) {
          pdfs.text <- pdf_text(path)
        }
        if (str_count(category, '.txt') == 1) {
          pdfs.text <- unlist(read_lines(file = path))
        }
      }
      # If it is a pdf that is uploaded in a zip
      # To be re-implemented
      # else if (str_count(category, "/") == 1) {
      #   
      #   # update this stuff.
      #   path <- paste("unzip/", category, sep = "")
      #   pos = regexpr('/', category)
      #   category <- substr(category, pos + 1, nchar(category))
      #   #pdfs.text[[category]] <- ozp_pdf_stemming(pdf_text(path))
      #   #pdfs.text[[category]] <- pdf_text(path)
      #   
      #   # Clean text
      #   #pdfs.text <- clean_text(pdfs.text, stemming)
      #   
      #   # New stemming here:
      #   #pdfs.text <- ozp_pdf_stemming(pdfs.text)
      #   # Save pdf text as category
      #   #allpdfs.text[[category]] <- pdfs.text
      #}
      ## RE-implement this feature later, when done cleaning up the original mess
      # If it is a category with pdfs or txt underneath it
      # else {
      #   positions <- which(grepl(paste("/", category, "/", sep = ""), pdfs, fixed = TRUE))
      #   
        # # Iterate over each pdf file of this category
        # for (k in 1:length(positions)) {
        #   category.pdf <- pdfs[positions[k]]
        #   path <- paste("unzip/", category.pdf, sep = "")
        #   pos <- gregexpr('/', category.pdf)
        #   pos <- pos[[1]][length(pos[[1]])]
        #   category.pdf <- substr(category.pdf, pos + 1, nchar(category.pdf))
        #   pdfs.text[[category.pdf]] <- pdf_text(path)
        # }
        
        # Clean text old version
        #pdfs.text <- clean_text(pdfs.text, stemming)
        # stemming
        #pdfs.text <- ozp_pdf_stemming(pdfs.text)
        # Save text of all pdfs to category
        #allpdfs.text[[category]] <- pdfs.text
      # }
      #clean the text and add it to its category
      allpdfs.text[[category]] <- clean_text(pdfs.text, stemming)
    }
  })
  return(allpdfs.text)
}

# 
# >> LonglistLoad <<
# Description: Loading the longlist from the excel sheet(s)
# Input: datapath to file
# Output: merged longlist with a languages per column
# 

load_longlist <- function(files){
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