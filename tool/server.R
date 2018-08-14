#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
options(java.parameters = "-Xmx2000m")
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
library(shinyBS)

library(xlsx) # Write to excel
library(readxl) # Read from excel
library(tm) # Text Mining
library(pdftools) # Text Extraction
library(SnowballC) # Stemming
library(wordcloud) # Word Cloud
library(stringr) # Count substring in string
library(textcat) # Detect language of text
library(rhandsontable) # Interactive tables
library(plotly) # Interactive plots

source("functions/parse longlist.R")
source("functions/keyword_generation.R")



shinyServer(function(input, output) {


  
  output$table.pdfs <- renderTable(width = "100%", hover = TRUE, {
    table <- data.frame(matrix(ncol = 1, nrow = 0))
    colnames(table) <- "No document(s) uploaded yet"
    
    if (is.null(input$pdfs)){
      return(table)
    }
    else{
      enable("wordCloudButtonPDF")
      removeClass("pdf1", "missing")
      removeClass("pdf2", "missing")
      removeClass("pdf3", "missing")
      removeClass("pdf4", "missing")
      removeClass("pdf5", "missing")
      if (!is.null(input$longlists)) {
        enable("tdmButton")
        enable("tdmDownload")
        enable("plotButton")
        removeClass("not-allowed", "not-allowed")
        enable("wordCloudButtonLonglist")
      }
      # set Header name for uploaded documents based on if 1 or n documents
      if(length(input$pdfs$name) == 1){
        colnames(table) <- "Uploaded document"
      }
      else{
        colnames(table) <- "Uploaded documents"
      }
      # list document names in the documents section of the HTML.
      for(pdf in 1:length(input$pdfs$name)){
        table[pdf,1] <- input$pdfs$name[pdf]
      }
      return(table)
    }
  })
  
  output$table.longlists <- renderTable(width = "100%", hover = TRUE, {
    table <- data.frame(matrix(ncol = 1, nrow = 0))
    colnames(table) <- "No Longlist file(s) uploaded yet"
    
    if (is.null(input$longlists)){
      return(table)
    }
    else{
      file.rename(input$longlists$datapath, paste(input$longlists$datapath, ".xlsx", sep=""))
      removeClass("excel1", "missing")
      removeClass("excel2", "missing")
      removeClass("excel3", "missing")
      removeClass("excel4", "missing")
      if(length(input$longlists$name) == 1){
        colnames(table) <- "Uploaded longlist file"
      }
      else{
        colnames(table) <- "Uploaded longlist files"
      }
      for(longlist in 1:length(input$longlists$name)){
        table[longlist,1] <- input$longlists$name[longlist]
      }
      return(table)
    }
  })

  output$exampleLongListDownload <- downloadHandler("examplelonglist.xlsx", content = function(path) { file.copy("examplelonglist.xlsx", path) })

  output$tdmDownload <- downloadHandler(
    filename = function() {
      score <- ""
      if(input$scoring == 1){
        score <- "[Count]"
      }
      else if(input$scoring == 2){
        score <- "[Frequency]"
      }
      else if(input$scoring == 3){
        score <- paste("[Relative - ", input$threshold, "]", sep="")
      }
      else {
        score <- "[Weighted]"
      }
      prefix <- ""
      if(input$title != ""){
        prefix <- paste(input$title," - ", sep="")
      }
      exportName <- paste(prefix,"Matrix ", score ,".xlsx",sep="")
      paste(exportName, sep='')
    },
    content = function(path) {
      saveTDM(getTDM(), path, readLonglists())
    }
  )
  
  output$logDownload <- downloadHandler(
    filename = function() {
      prefix <- ""
      if(input$title != ""){
        prefix <- paste(input$title," - ", sep="")
      }
      exportName <- paste(prefix,"Log",".txt",sep="")
      paste(exportName, sep='')
    },
    content = function(path) {
      file.copy("outfile.txt", path)
    }
  )
  
  observeEvent(input$wordCloudButtonPDF, {
    shinyjs::hide("iconWordCloudPDFEmpty")
    shinyjs::show("iconWordCloudPDFLoad")
    output$wordCloudPlotPDF <- renderPlot({
      printWordCloudPDF()
      shinyjs::hide("placeholderWordCloudPDF")
    })
    shinyjs::show("wordCloudPlotPDF")
  })
  
  observeEvent(input$wordCloudButtonLonglist, {
    shinyjs::hide("iconWordCloudLonglistEmpty")
    shinyjs::show("iconWordCloudLonglistLoad")
    output$wordCloudPlotLonglist <- renderPlot({
      printWordCloudLonglist()
      shinyjs::hide("placeholderWordCloudLonglist")
    })
    shinyjs::show("wordCloudPlotLonglist")
    shinyjs::show("logDownload")
  })
  
  observeEvent(input$tdmButton, {
    shinyjs::hide("iconTDMEmpty")
    shinyjs::show("iconTDMLoad")
    output$tdm <- renderDataTable({
      getTDM()
    }, options = list(pageLength = 10, scrollX = TRUE), 
    list(shinyjs::hide("placeholderTDM")
    ))
    shinyjs::show("tdm")
    shinyjs::show("logDownload")
  })
  
  observeEvent(input$plotButton, {
    shinyjs::show("plot")
    shinyjs::hide("iconPlotEmpty")
    shinyjs::show("iconPlotLoad")
    shinyjs::show("scoreBox")
    output$table.plot <- renderRHandsontable({
      rhandsontable(getPlotTDM(), rowHeaders = TRUE) %>%
        hot_validate_numeric(col = 2, min = 0, max = 10, allowInvalid = TRUE) %>%
        hot_validate_numeric(col = 3, min = 0, max = 10, allowInvalid = TRUE) %>%
        hot_col(col = 1, readOnly = TRUE, colWidths = 600) %>%
        hot_col(col = 2, halign = "htCenter", format = "0.0") %>%
        hot_col(col = 3, halign = "htCenter", format = "0.0") %>%
        hot_col(col = 4, halign = "htCenter") %>%
        hot_cols(columnSorting = TRUE)
    })
    shinyjs::show("logDownload")
  })
  
  observeEvent(input$table.plot, {
    shinyjs::show("plot")
    shinyjs::hide("placeholderPlot")
    output$plot <- renderPlotly({
      generatePlot(input$table.plot, 20)
    })
  })
  
    readDocuments <- reactive({
    documentsLoad(input$pdfs)
  })
  
  readLonglists <- reactive({
    longlistLoad(input$longlists)
  })
  
  wordCloudPDF <- reactive({
    prepareWordCloudPDF(readDocuments())
  })
  
  wordCloudLonglist <- reactive({
    prepareWordCloudLonglist(getTDM())
  })
  
  printWordCloudPDF <- reactive({
    generateWordCloud(wordCloudPDF(), input$wordCloudPDFNumber)
  })
  
  printWordCloudLonglist <- reactive({
    generateWordCloud(wordCloudLonglist(), input$wordCloudLonglistNumber)
  })

    # aanmaken term document matrix # ralph
    getTDM <- reactive({
    createTDM(readDocuments(), readLonglists(), input$scoring, input$threshold, input$longlistoption)
  })
  
  getPlotTDM <- reactive({
    preparePlotTDM(getTDM())
  })
  
  printPlot <- reactive({
    generatePlot(input$table.plot, 20)
  })
    
  
  # 
  # >> prepareWordCloudPDF <<
  # Input: text from pdf(s)
  # Output: word frequency list
  #  
  prepareWordCloudPDF <- function(pdfs.text){
    
    withProgress(message = 'Generating Word Cloud', value = 0, {
      
      # Create corpus
      corpus <- Corpus(VectorSource(pdfs.text))
      incProgress(1/2)
      
      # Create TermDocumentMatrix
      tdm <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE,
                                                       stopwords = TRUE,
                                                       tolower = TRUE,
                                                       stemming = FALSE,
                                                       removeNumbers = TRUE,
                                                       bounds = list(global = c(1, Inf))))
      
      incProgress(1/2)
      
      # Create the table with the correct names
      result <- as.matrix(tdm)
      result <- rowSums(result)
    })
    return(result)
  }
  
  
  
  # 
  # >> prepareWordCloudLonglist <<
  # Input: term document matrix
  # Output: term frequency list
  # 
  prepareWordCloudLonglist <- function(tdm){
    frequency <- c(tdm[,2])
    synonym = list()
    
    for(i in 1:nrow(tdm)){
      split <- strsplit(toString(tdm[i,1]), " / ")
      split <- split[[1]][1]
      synonym <- c(synonym, split)
    }
    
    names(frequency) <- synonym

    return(frequency)
  }
  
  # 
  # >> generateWordCloud <<
  # Input: term / word frequency list and number of term / words to display
  # Output: word cloud
  #
  
  generateWordCloud <- function(frequency, number){
    
    # Check if the number of terms to show in the word cloud is not bigger than the number of words to show
    if(number > length(frequency)){
      number <- length(frequency)
    }
    
    # Sort the frequencies
    frequency <- sort(frequency, decreasing = TRUE)
    
    words <- names(frequency)
    return(wordcloud(words[1:number], frequency[1:number], min.freq = 1, random.order=FALSE, rot.per=0.35, use.r.layout = FALSE, colors=brewer.pal(4, "Paired")))
  }
    
  # 
  # >> createTDM <<
  # Input: text from pdf(s), longlist terms, scoring scheme and threshold
  # Output: term document matrix
    # 
    createTDM <- function(allpdfs.text, longlist, scheme, threshold, longlist.mode) {
        withProgress(message = 'Generating Table', value = 0, {
        longlist.mode
        # set topic and description column number.
        # this was done to increase the readability of the code
        topic_col_nr = 1
        description_col_nr = 2

            columns <- NULL



        # test optimalization effort
        optimized_descriptions = list()

            for (row in 1:nrow(longlist)) {

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
            } else
                optimized_descriptions = c(optimized_descriptions, list(terms.list))
        }
        else {
            if (length(optimized_descriptions) == 0) {
                optimized_descriptions = list("")
            } else {
                optimized_descriptions = c(optimized_descriptions, "")
            }
        }
        } else {
             extracted.description <- longlist[[row, description_col_nr]]
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
            stemmed.list=c(stemmed.list,stemmed.keyword)
        }

            #for (keyword in 1:length(splitted.string.list)) {
            #stemmed.keyword = paste(SnowballC::wordStem(splitted.string.list[[keyword]], language = "english"))
            #stemmed.list=c(stemmed.list,stemmed.keyword)

                ##stemmed.list=c(stemmed.list,paste(SnowballC::wordStem(splitted.string.list[[keyword]],language = "english")))
            #}

         terms.list=stemmed.list   
        if (length(optimized_descriptions) == 0) {
        optimized_descriptions = list(terms.list)
        } else
        optimized_descriptions = c(optimized_descriptions, list(terms.list))
        }
        else {
        if (length(optimized_descriptions) == 0) {
        optimized_descriptions = list("")
        } else {
        optimized_descriptions = c(optimized_descriptions, "")
        }
        }
        }
        }
            
        # Iterate over all categories
        for (cat in 1:length(names(allpdfs.text))) {

            catName <- names(allpdfs.text)[cat]

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
        sink("outfile.txt")

            # Iterate over every category 
        for (l in 1:length(columns)) {

            # introduce loop here
            pdfs.text <- allpdfs.text[[l]]
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
                                term.frequency <- getFrequency(term, pdfPages, categoryName, pdfName, language)
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
            catTDM <- addScore(catTDM, scheme, threshold, TRUE)

            # Add the score column of the category table to the main table
            tdm[1:nrow(tdm), l + 1] <- catTDM[1:nrow(catTDM), 2]
        }

            # After all the categories columns are added, add the score column to the main tdm
        tdm <- addScore(tdm, scheme, threshold, FALSE)

            # Set the correct names again
        colnames(tdm) <- c("Longlist", "Score", columns)

        })
        return(tdm)
    }
  
  # 
  # >> addScore <<
  # Input: term document matrix, scoring scheme, threshold and a boolean whether it is a category tdm or the main tdm
  # Output: term document matrix with a score column
  # 
  
  addScore <- function(tdm, scheme, threshold, category){
    
    # If the scoring scheme is 'count'
    if(scheme == 1){
      
      # If the tdm has only one category/pdf
      if(ncol(tdm) == 2){
        
        # Add a score column with a total of all the columns
        tdm <- as.data.frame(append(tdm, list(Score = tdm[,2]), after = 1))
        
        # Iterate over each topic in the longlist
        for(i in 1:nrow(tdm)){
          
          # If the topic occurs in at least ones, change it to 1 (so 0 stays 0)
          if(tdm[i,2] > 0){
            tdm[i,2] <- 1
          }
        }
      }
      
      # If the tdm has more than one category/pdf
      else{
        
        # Add a score column with 1 if at least one other column is bigger than 0
        tdm <- as.data.frame(append(tdm, list(Score = rowSums(tdm[,2:ncol(tdm)] > 0)), after = 1))
      }
    }
    
    # If the scoring scheme is 'frequency' or if the scoring scheme is 'weighted' & it is a category table.
    # We do not want to lower the frequency twice (we do this only for the main table)
    else if(scheme == 2 || (scheme == 4 && category)){
      
      # If the tdm has only one category/pdf
      if(ncol(tdm) == 2){
        
        # Add a score column that is equal to the one column
        tdm <- as.data.frame(append(tdm, list(Score = tdm[,2]), after = 1))
      }
      
      # If the tdm has more than one category/pdf
      else{
        
        # Add a score column with a total of all the columns
        tdm <- as.data.frame(append(tdm, list(Score = rowSums(tdm[,2:ncol(tdm)])), after = 1))
      }
    }
    
    # If the scoring scheme is 'relative'
    else if (scheme == 3){
      
      # Add a score column with a total of all the columns
      tdm <- as.data.frame(append(tdm, list(Score = tdm[,2]), after = 1))
      
      # Iterate over each row in the matrix
      for(row in 1:nrow(tdm)){
        score <- 0
        
        # Iterate over each pdf in the matrix
        for(pdf in 3:ncol(tdm)){
          
          value <- tdm[row,pdf]
          
          # If it is a category table
          if(category){
            
            # Count 1 if value is bigger or equal to the threshold
            if(value >= threshold){
              score <- score + 1
            }
          }
          
          # If it is the main table
          else {
            
            # Count 1 if value is bigger than 0
            # Because for each category we already did the bigger or equal to the threshold check
            if(value > 0){
              score <- score + 1
            }
          }
        }
        
        # Save the score to the Score column
        tdm[row,2] <- score
      }
    }
    
    # If the scoring scheme is 'weighted'
    else{
      
      # If the tdm has only one category/pdf
      if(ncol(tdm) == 2){
        
        # Add a score column that is equal to the one column
        tdm <- as.data.frame(append(tdm, list(Score = tdm[,2]), after = 1))
      }
      
      # If the tdm has more than one category/pdf
      else{
        
        # Get the total frequency for each longlist topic
        tdm.frequency <- list(rowSums(tdm[,2:ncol(tdm)]))
        
        # Get the number of categories/pdfs for each longlist topic for which the frequency is bigger than 0
        tdm.count <- list(rowSums(tdm[,2:ncol(tdm)] > 0))
        
        # Divide the total frequency for each longlist topic by the total number of categories/pdfs
        tdm.mixed <- mapply('/', tdm.frequency, (ncol(tdm) - 1))
        
        # Multiply the above by the number of categories/pdfs with a score higher than 0
        tdm.mixed <- list(Score = unlist(tdm.mixed)*unlist(tdm.count))
        
        # Add the score column to the tdm
        tdm <- as.data.frame(append(tdm, tdm.mixed, after = 1))
      }
    }
    
    return(tdm)
  }
  
  
  
  # 
  # documentsLoad: unzipping files and loading the text from the pdf file(s)
  # Input: folder path
  # Output: list with all the text in the pdf(s) per category
  # 
  
  documentsLoad <- function(files){
    withProgress(message = 'Reading documents', value = 0, {
      
      pdfs <- c() # create vector
      categories <- c() #create vector
      
      # Iterate over all the uploaded files
      number.files <- length(files[,1])
      for(i in 1:number.files){
        fileName <- files[[i, 'name']]
        incProgress(0, detail = "Scanning documents")
        
        # If the file is a .zip, unzip it
        if (grepl(".zip", fileName, fixed = TRUE)) {

          zipFiles <- unzip(files[[i, 'datapath']], list = TRUE)


          # Iterate over each file in the zip
          for(n in 1:nrow(zipFiles)){
            zipContent <- zipFiles[n, "Name"]
            #observe(print(zipContent))
            zipName <- gsub('.zip','',fileName)
            mainFolder <- paste(zipName,"/",sep="")
            
            # Only check the file in the zip if it is valid
            if(str_count(zipContent, "__MACOSX") == 0 && str_count(zipContent, ".DS_Store")== 0 && mainFolder != zipContent){
              
              # If the file is a pdf, read the pdf #bad comment
              if(grepl(".pdf", zipContent)){
                if(str_count(zipContent, "/") == 1){ # possible category stuff # oftewel er is een folder structure aanwezig # >=1
                  categories[length(categories) + 1] <- zipContent # insert value into vector.
                }
                pdfs[length(pdfs) + 1] <- zipContent
              }
              # If the file within the zip is an invalid format, ignore it
              else if (grepl(".xls", zipContent, fixed=TRUE) || grepl(".xlsx", zipContent, fixed=TRUE) || grepl(".doc", zipContent, fixed=TRUE) || grepl(".docx", zipContent, fixed=TRUE) || grepl(".jpg", zipContent, fixed=TRUE) || grepl(".png", zipContent, fixed=TRUE) || grepl(".iso", zipContent, fixed=TRUE) || grepl(".txt", zipContent, fixed=TRUE)) {
                # Do nothing
              }
              # If the file is a folder, add as category
              else{
                zipContent <- gsub('/','',gsub(zipName,'',zipContent))
                categories[length(categories) + 1] <- zipContent
              }
            }
          }
          unzip(files[[i, 'datapath']], exdir = "unzip", overwrite = TRUE)
        }
        # If the file is a .pdf add it as an own category and read the pdf
        else if(grepl(".pdf", fileName)){
          categories[length(categories) + 1] <- paste(fileName)
          pdfs[length(pdfs) + 1] <- paste(fileName)
        }
        # Documents other than pdf or zip are ignored
      }
      
      allpdfs.text = list()

      #observe(print(typeof(categories)))
      
      # Iterate over each category
      for(number in 1:length(categories)) {
        category <- categories[number]
        pdfs.text <- NULL
        
        incProgress(1/length(categories), detail = category)
        # If it is a pdf that is directly uploaded
        if(str_count(category, "/") == 0  && str_count(category, ".pdf") == 1) {
          path <- files[[which(grepl(category, files$name, fixed=TRUE)),'datapath']]

          # stem the PDF
          pdfs.text[[category]] <- ozp_pdf_stemming(pdf_text(path))

          # Clean text
          # Save pdf text as category
          allpdfs.text[[category]] <- pdfs.text
        }
        # If it is a pdf that is uploaded in a zip
        else if (str_count(category, "/") == 1) {

          # update this stuff.
          path <- paste("unzip/",category,sep="")
          pos = regexpr('/', category)
          category <- substr(category, pos+1, nchar(category))
          pdfs.text[[category]] <- ozp_pdf_stemming(pdf_text(path))
          
          # Clean text
          pdfs.text <- cleanText(pdfs.text)

          # New stemming here:
          pdfs.text <- ozp_pdf_stemming(pdfs.text)
          # Save pdf text as category
          allpdfs.text[[category]] <- pdfs.text
        }
        # If it is a category with pdfs underneath it
        else{
          positions <- which(grepl(paste("/",category,"/",sep=""), pdfs, fixed=TRUE))
          
          # Iterate over each pdf file of this category
          for(k in 1:length(positions)){
            category.pdf <- pdfs[positions[k]]
            path <- paste("unzip/",category.pdf,sep="")
            pos <- gregexpr('/', category.pdf)
            pos <- pos[[1]][length(pos[[1]])]
            category.pdf <- substr(category.pdf, pos+1, nchar(category.pdf))
            pdfs.text[[category.pdf]] <- pdf_text(path)
          }
          
          # Clean text old version
          pdfs.text <- cleanText(pdfs.text)
          # stemming
          pdfs.text<-ozp_pdf_stemming(pdfs.text)
          # Save text of all pdfs to category
          allpdfs.text[[category]] <- pdfs.text
        }
      }
    })
    return(allpdfs.text)
  }
  
  
  # 
  # longlistLoad: loading the longlist from the excel sheet(s)
  # Input: folder path
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
  # >> cleanText <<
  # Input: pdf text
  # Output: cleaned pdf text
  #
  
  cleanText <- function(pdfs.text){
    
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
  # >> getFrequency <<
  # Input: term, pdf text, category name, pdf name, language
  # Output: the number of times the term occures in the pdf 
  #
  
  getFrequency <- function(term, pdf, categoryName, pdfName, language){
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
  # >> saveTDM <<
  # Input: tdm, path and longlist (for topic category names)
  # Output: save excel file to path
  #
  
  saveTDM <- function(tdm, path, longlist){
    withProgress(message = 'Saving Matrix', value = 0, {
      
      # Create empty excel file
      excel <- createWorkbook(type="xlsx")
      sheet <- createSheet(excel, sheetName="Longlist Results")
      
      
      # Define Layout
      # Define borders
      border.title <- Border(color = "black", pen = c("BORDER_MEDIUM", "BORDER_MEDIUM", "BORDER_THIN", "BORDER_THIN"), position = c("TOP", "BOTTOM", "LEFT", "RIGHT"))
      border.normal <- Border(color = "black", pen = c("BORDER_THIN", "BORDER_THIN", "BORDER_MEDIUM", "BORDER_MEDIUM"), position = c("TOP", "BOTTOM", "LEFT", "RIGHT"))
      border.score <- Border(color = "black", pen = "BORDER_MEDIUM", position = c("TOP", "BOTTOM", "LEFT", "RIGHT"))
      
      # Define allignments
      aligntment.columns <- Alignment(wrapText = TRUE, rotation = 60)
      alignment.list <- Alignment(wrapText = TRUE)
      alignment.standard <- Alignment(horizontal = "ALIGN_CENTER", vertical = "VERTICAL_CENTER", wrapText = TRUE)
      alignment.categorie <- Alignment(horizontal = "ALIGN_CENTER", wrapText = TRUE)
      
      # Define fills
      fill.gray <- Fill(foregroundColor = "#BFBFBF")
      fill.lightgray <- Fill(foregroundColor = "#D9D9D9")
      fill.white <- Fill(foregroundColor = "white")
      fill.lightgreen <- Fill(foregroundColor = "#ECF0E0")
      fill.lightblue <- Fill(foregroundColor = "#DFE9EF")
      fill.red <- Fill(foregroundColor = "#E7726F")
      fill.green1 <- Fill(foregroundColor = "#FCEC92")
      fill.green2 <- Fill(foregroundColor = "#CFDC8D")
      fill.green3 <- Fill(foregroundColor = "#B9D48A")
      fill.green4 <- Fill(foregroundColor = "#7ABC81")
      
      # Define fonts
      font.big <- Font(excel, heightInPoints = "16", name = "Calibri")
      font.normal <- Font(excel, heightInPoints = "10", name = "Calibri")
      
      style.title.score <- CellStyle(excel, alignment = aligntment.columns, border = border.score, fill = fill.lightgray, font = font.big)
      style.title.first <- CellStyle(excel, border = border.score, fill = fill.white, font = font.normal)
      style.title <- CellStyle(excel, alignment = aligntment.columns, border = border.title, fill = fill.lightgreen, font = font.normal)
      style.title.bundle <- CellStyle(excel, alignment = aligntment.columns, border = border.title, fill = fill.lightblue, font = font.normal)
      style.title.categorie <- CellStyle(excel, alignment = alignment.categorie, border = border.score, fill = fill.gray, font = font.normal)
      
      style.list.score <- CellStyle(excel, alignment = alignment.standard, border = border.normal, font = font.normal)
      style.list.score.green1 <- CellStyle(excel, alignment = alignment.standard, border = border.normal, font = font.normal, fill = fill.green1)
      style.list.score.green2 <- CellStyle(excel, alignment = alignment.standard, border = border.normal, font = font.normal, fill = fill.green2)
      style.list.score.green3 <- CellStyle(excel, alignment = alignment.standard, border = border.normal, font = font.normal, fill = fill.green3)
      style.list.score.green4 <- CellStyle(excel, alignment = alignment.standard, border = border.normal, font = font.normal, fill = fill.green4)
      style.list.score.red <- CellStyle(excel, alignment = alignment.standard, border = border.normal, font = font.normal, fill = fill.red)
      style.list.first <- CellStyle(excel, alignment = alignment.list, border = border.normal, font = font.normal)
      style.list <- CellStyle(excel, alignment = alignment.standard, border = border.normal, font = font.normal)
      style.list.categorie <- CellStyle(excel, alignment = alignment.standard, border = border.score, fill = fill.gray, font = font.normal)
      
      # Set column styles
      colstyle <- list('1' = style.list.first, '2' = style.list.score)
      for(i in 3:ncol(tdm)){
        colstyle[[paste(i)]] <- style.list
      }
      
      # Store TermDocumentMatrix to excel file
      addDataFrame(tdm, sheet, col.names=TRUE, row.names=FALSE,
                   startRow=1, startColumn=2, colStyle=colstyle, colnamesStyle=style.title)
      
      # Set row height
      row <- getRows(sheet, rowIndex = 1)
      setRowHeight(row, 90)
      
      # Set column width
      setColumnWidth(sheet, colIndex = 2, colWidth = 50)
      
      # Set individual cells
      row <- getRows(sheet, rowIndex = 1)
      cells <- getCells(row)
      setCellStyle(cells[[2]], style.title.score)
      setCellStyle(cells[[1]], style.title.first)
      for(i in 3:ncol(tdm)){
        if (grepl("[Category]", names(tdm)[i], fixed=TRUE)){
          cells <- getCells(getRows(sheet, rowIndex = 1))
          setCellStyle(cells[[i]], style.title.bundle)
        }
      }
      
      incProgress(1/2)
      
      # Set colors for score
      for(i in 1:nrow(tdm)+1){
        row <- getRows(sheet, rowIndex = i)
        cells <- getCells(row)
        value <- getCellValue(cells[[2]])
        if(value > 0 & value < 5){
          setCellStyle(cells[[2]], style.list.score.green1)
        }
        else if(value > 4 & value < 10){
          setCellStyle(cells[[2]], style.list.score.green2)
        }
        else if(value > 9 & value < 20){
          setCellStyle(cells[[2]], style.list.score.green3)
        }
        else if(value > 19){
          setCellStyle(cells[[2]], style.list.score.green4)
        }
        else{
          setCellStyle(cells[[2]], style.list.score.red)
        }
      }
      
      # Freeze cells
      createFreezePane(sheet, rowSplit = 2, colSplit = 4, startRow=NULL, startColumn=NULL)
      
      # Set zoom
      setZoom(sheet, numerator = 130, denominator = 100)
      
      # Set print area
      setPrintArea(excel, sheetIndex = 1, startColumn = 1, endColumn = ncol(tdm), startRow = 1, endRow = nrow(tdm))
      
      # Add categories
      longlist <- as.matrix(longlist[1])
      addDataFrame(longlist, sheet, col.names=TRUE, row.names=FALSE, startRow=1, startColumn=1, colStyle = list('1' = style.list.categorie), colnamesStyle=style.title.categorie)
      
      # Add the topic category names to the first column. Merge the category cells for topics that are part of the same category 
      lastWord = ""
      start = 0
      
      for(i in 1:nrow(longlist)){
        word = longlist[i]
        if(word != lastWord & !is.na(word)){
          if(start != 0){
            addMergedRegion(sheet, start+1, i, 1, 1)
          }
          start = i
          lastWord = word
        }
      }
      addMergedRegion(sheet, start+1, nrow(longlist)+1, 1, 1)
      
    })
    
    #Save the file
    saveWorkbook(excel, path)
  }
  
  
  
  #
  # >> generatePlot <<
  # Input: term frequency list and number to display
  # Output: plot
  #
  
  generatePlot <- function(tdm, number){
    # Translate the table in the user interface to a R data frame
    tdm <- hot_to_r(tdm)
    
    # Add a column to the tdm
    tdm$Synonym <- NULL
    
    # Iterate over each topic in the tdm and store the first synonym in the newly created column
    for(row in 1:nrow(tdm)){
      split <- strsplit(toString(tdm[row,1]), " / ")
      split <- split[[1]][1]
      tdm[row,5] <- split
    }
    
    # Create a scatter plot for the tdm
    # Use only the first synonym of a topic to show in the plot, but show all synoynms of a topic when the users hovers over it
    plot <- plot_ly(x = tdm[tdm[,4] == TRUE,3], y = tdm[tdm[,4] == TRUE,2], type = 'scatter', mode = 'markers', text = tdm[tdm[,4] == TRUE,5], hovertext = tdm[tdm[,4] == TRUE,1],
                    hoverinfo = 'x+y+text', showlegend = FALSE, marker = list(size = 10,
                                                                              color = 'rgba(184, 230, 255, .9)',
                                                                              line = list(color = 'rgba(0, 77, 153, .8)',
                                                                                          width = 2))) %>%
      add_text(textfont = list(
        family = "sans serif",
        size = 14,
        color = toRGB("grey50")),
        textposition = "bottom center") %>%
      
      layout(title = 'Materiality Matrix',
             xaxis = list(
               nticks = 10,
               range = c(-1, 11),
               title = "Importance internal stakeholders",
               titlefont = list(
                 family = 'Arial, sans-serif',
                 size = 15,
                 color = 'black'
               ),
               showexponent = 'All'
             ),
             yaxis = list(
               nticks = 10,
               range = c(-1, 11),
               title = "Importance external stakeholders",
               titlefont = list(
                 family = 'Arial, sans-serif',
                 size = 15,
                 color = 'black'
               ),
               showexponent = 'All'
             )
      )
    return(plot)
  }
  
  
  
  #
  # >> preparePlotTDM <<
  # Input: term document matrix
  # Output: matrix with topics, external score, internal scofre and show columns
  #
  
  preparePlotTDM <- function(tdm){
    
    # Only take the topics and the score column
    tdm <- tdm[,1:2]
    
    # The score column is for the external axis
    colnames(tdm)[2] <- "External"
    
    # Add the internal axis column
    tdm$Internal <- rep(0,nrow(tdm))
    
    # Order the tdm on the external axis score
    tdm <- tdm[order(tdm$External, decreasing=TRUE), ]
    
    # Add a column to store whether to show a topic in the matrix or not -> default is FALSE
    tdm$Show <- rep(FALSE,nrow(tdm))
    
    # Normalize the values to a range between 10 and 0. Highest score gets a 10
    highestvalue <- tdm[1,2]
    for(row in 1:nrow(tdm)){
      score <- tdm[row,2]/highestvalue*10
      tdm[row,2] <- score
      
      # Only set topics to show is TRUE when the external axis score is higher than 1
      if(score > 1){
        tdm[row,4] <- TRUE
      }
    }
    
    return(tdm)
  }
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

})