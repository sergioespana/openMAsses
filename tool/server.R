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
library(FactoMineR) #PCA for dimension reduction
library(reticulate)

source("functions/parse longlist.R")
source("functions/generator_keywords.R")
source("functions/generator_wordcloud.R")
source("functions/generator_tdm.R")
source("functions/generator_matrix.R")
source("functions/data_loaders.R")


shinyServer(function(input, output) {
  
  #Creates a table of all pdf files that were uploaded, enables analysis buttons and writes name down
  #for overview
  output$searchTermsTable <- renderTable(width = '100%', hover = TRUE, {
    
    table <- form_term_table(input$searchTermsInput)
    
    colnames(table) <- c('Category','Terms')
    return(table)
  })
  
  output$table.pdfs <- renderTable(width = "100%", hover = TRUE, {
    table <- data.frame(matrix(ncol = 1, nrow = 0))
    colnames(table) <- "No document(s) uploaded yet"
    
    if (is.null(input$pdfs)) {
      return(table)
    }
    else {
      enable("wordCloudButtonPDF")
      removeClass("pdf1", "missing")
      removeClass("pdf2", "missing")
      removeClass("pdf3", "missing")
      removeClass("pdf4", "missing")
      removeClass("pdf5", "missing")
      if (!is.null(input$longlists)) {
        enable("tdmButton")
        enable("tdmDownload")
        removeClass("not-allowed", "not-allowed")
        enable("wordCloudButtonLonglist")
        if ((!is.null(input$media)) || ((file.exists('twitter.txt')) && (file.exists('reddit.txt')) && (file.exists('news.txt')))) {
          enable("plotButtonMatrix")
        }
      }
      # set Header name for uploaded documents based on if 1 or n documents
      if (length(input$pdfs$name) == 1) {
        colnames(table) <- "Uploaded document"
      }
      else {
        colnames(table) <- "Uploaded documents"
      }
      # list document names in the documents section of the HTML.
      for (pdf in 1:length(input$pdfs$name)) {
        table[pdf, 1] <- input$pdfs$name[pdf]
      }
      return(table)
    }
  })
  
  #Creates a table of all media files that were uploaded, enables analysis buttons and writes name down
  #for overview
  output$table.media <- renderTable(width='100%', hover = TRUE, {
    table <- data.frame(matrix(ncol = 1, nrow = 0))
    colnames(table) <- "No media file(s) uploaded yet"
    
    if (is.null(input$media)){
      table <- autoLoad_media()
      if (!is.null(table)) {
        enable('wordCloudButtonTwitter')
        enable('wordCloudButtonNews')
        enable('wordCloudButtonReddit')
        removeClass('media1', 'missing')
        removeClass('media2', 'missing')
        removeClass('media3', 'missing')
        removeClass('media4', 'missing')
        removeClass('media5', 'missing')
        removeClass('media6', 'missing')
        if (!is.null(input$longlists)) {
          removeClass("not-allowed", "not-allowed")
          enable("tdmMediaButton")
          enable("tdmMediaDownload")
          if (!is.null(input$pdfs)){
            enable('plotButtonMatrix')
          }
        }
        removeClass("not-allowed", "not-allowed")
      }
      return(table)
    }
    else {
      enable('wordCloudButtonTwitter')
      enable('wordCloudButtonNews')
      enable('wordCloudButtonReddit')
      removeClass('media1', 'missing')
      removeClass('media2', 'missing')
      removeClass('media3', 'missing')
      removeClass('media4', 'missing')
      removeClass('media5', 'missing')
      removeClass('media6', 'missing')
      if (!is.null(input$longlists)) {
        removeClass("not-allowed", "not-allowed")
        enable("tdmMediaButton")
        enable("tdmMediaDownload")
        if (!is.null(input$pdfs)){
          enable('plotButtonMatrix')
        }
      }
      removeClass("not-allowed", "not-allowed")
      if (length(input$media$name) == 1) {
        colnames(table) <- "Uploaded document"
      }
      else {
        colnames(table) <- "Uploaded documents"
      }
      # list document names in the documents section of the HTML.
      for (media in 1:length(input$media$name)) {
        table[media, 1] <- input$media$name[media]
      }
      print(table)
      return(table)
    }
  })
  
  autoLoad_media <- reactive({
    if ((file.exists('twitter.txt')) && (file.exists('reddit.txt')) && (file.exists('news.txt'))) {
      values <- c('news.txt', 'reddit.txt', 'twitter.txt')
      table <- matrix(values, nrow = 3, ncol = 1)
      colnames(table) <- 'Uploaded media files'
      return(table)
    }
    else {
      return()
    }
  })
  
  #Creates a table of all longlist files that were uploaded, enables analysis buttons and writes name down
  #for overview
  output$table.longlists <- renderTable(width = "100%", hover = TRUE, {
    table <- data.frame(matrix(ncol = 1, nrow = 0))
    colnames(table) <- "No Longlist file(s) uploaded yet"
    
    if (is.null(input$longlists)) {
      return(table)
    }
    else {
      file.rename(input$longlists$datapath, paste(input$longlists$datapath, ".xlsx", sep = ""))
      removeClass("excel1", "missing")
      removeClass("excel2", "missing")
      removeClass("excel3", "missing")
      removeClass("excel4", "missing")
      removeClass("excel5", "missing")
      removeClass("excel6", "missing")
      if (length(input$longlists$name) == 1) {
        colnames(table) <- "Uploaded longlist file"
      }
      else {
        colnames(table) <- "Uploaded longlist files"
      }
      for (longlist in 1:length(input$longlists$name)) {
        table[longlist, 1] <- input$longlists$name[longlist]
      }
      return(table)
    }
  })
  
  #Downloads an example of a longlist, this will be added to the github later
  output$exampleLongListDownload <- downloadHandler("examplelonglist.xlsx", content = function(path) { file.copy("examplelonglist.xlsx", path) })
  
  #Handles all the radio buttons' input?
  output$tdmDownload <- downloadHandler(
    filename = function() {
      score <- ""
      if (input$scoring == 1) {
        score <- "[Count]"
      }
      else if (input$scoring == 2) {
        score <- "[Frequency]"
      }
      else if (input$scoring == 3) {
        score <- paste("[Relative - ", input$threshold, "]", sep = "")
      }
      else {
        score <- "[Weighted]"
      }
      prefix <- ""
      if (input$title != "") {
        prefix <- paste(input$title, " - ", sep = "")
      }
      exportName <- paste(prefix, "Matrix ", score, ".xlsx", sep = "")
      paste(exportName, sep = '')
    },
    #Idk what this does yet, save the TDM for download I guess
    content = function(path) {
      saveTDM(get_TDM(), path, read_longlists())
    }
  )
  
  #Idk what this does either, creates some sort of downloadable file
  output$logDownload <- downloadHandler(
    filename = function() {
      prefix <- ""
      if (input$title != "") {
        prefix <- paste(input$title, " - ", sep = "")
      }
      exportName <- paste(prefix, "Log", ".txt", sep = "")
      paste(exportName, sep = '')
    },
    #Idk what this does either, creates some sort of downloadable file
    content = function(path) {
      file.copy("outfile.txt", path)
    })
  
  #When the search button is clicked, it writes all the search terms to the 
  #a text file and starts the analysis
  observeEvent(input$collectButton, {

    #If an old search file still exists, remove it
    #if (file.exists('search_terms.csv')) {
     # unlink('search_terms.csv')
      #file.remove('search_terms.csv')
    #}

    #before writing a new one
    write.csv(form_term_table(input$searchTermsInput)[,2], file='search_terms.csv', row.names = FALSE, col.names = FALSE)
    print(input$searchTermsInput)
    
    #call to pyton.exe
    use_python('C:/Python37/python')
    use_virtualenv('C:/Users/Melchior/Pycharmprojects/Thesisv3/venv/')

    #system("python C:/Users/Melchior/Pycharmprojects/Thesisv3/venv/main.py")
    #system2("python C:/Users/Melchior/Pycharmprojects/Thesisv3/Venv/main.py", args = c("collect", '5'), stdout = TRUE, stderr = TRUE)
    print('system is called')
  })
  
  observeEvent(input$runButton, {
    system2("C:/Users/Melchior/Desktop/off-line tool copy/Tool v1/tool/dist/main/main.exe", args = 'analyze')
  })
  
  #A set of button event handlers
  #This one is for PDF word clouds
  observeEvent(input$wordCloudButtonPDF, {
    shinyjs::hide("iconWordCloudPDFEmpty") ##We mean wordcloudpeers, but due to old version it pdf is still used
    shinyjs::show("iconWordCloudPDFLoad")
    output$wordCloudPlotPDF <- renderPlot({
      print_wordCloudPeers()
      shinyjs::hide("placeholderWordCloudPDF")
    })
    shinyjs::show("wordCloudPlotPDF")
  })
  
  #For News word clouds
  observeEvent(input$wordCloudButtonNews, {
    shinyjs::hide("iconWordCloudNewsEmpty")
    shinyjs::show("iconWordCloudNewsLoad")
    output$wordCloudPlotNews <- renderPlot({
      print_wordCloudNews()
      shinyjs::hide("placeholderWordCloudNews")
    })
    shinyjs::show("wordCloudPlotNews")
  })
  
  #For News word clouds
  observeEvent(input$wordCloudButtonReddit, {
    shinyjs::hide("iconWordCloudRedditEmpty")
    shinyjs::show("iconWordCloudRedditLoad")
    output$wordCloudPlotReddit <- renderPlot({
      print_wordCloudReddit()
      shinyjs::hide("placeholderWordCloudReddit")
    })
    shinyjs::show("wordCloudPlotReddit")
  })
  
  #For Twitter media word clouds
  observeEvent(input$wordCloudButtonTwitter, {
    shinyjs::hide("iconWordCloudTwitterEmpty")
    shinyjs::show("iconWordCloudTwitterLoad")
    output$wordCloudPlotTwitter <- renderPlot({
      print_wordCloudTwitter()
      shinyjs::hide("placeholderWordCloudTwitter")
    })
    shinyjs::show("wordCloudPlotTwitter")
  })
  
  #For longlist word clouds
  observeEvent(input$wordCloudButtonLonglist, {
    shinyjs::hide("iconWordCloudLonglistEmpty")
    shinyjs::show("iconWordCloudLonglistLoad")
    output$wordCloudPlotLonglist <- renderPlot({
      print_wordCloudLonglist()
      shinyjs::hide("placeholderWordCloudLonglist")
    })
    shinyjs::show("wordCloudPlotLonglist")
    shinyjs::show("logDownload")
  })
  
  #For the TDM, in tab Term Document Matrix
  observeEvent(input$tdmButton, {
    shinyjs::hide("iconTDMEmpty")
    shinyjs::show("iconTDMLoad")
    output$tdm <- renderDataTable({
      get_TDM()
    },
    options = list(pageLength = 10, scrollX = TRUE),
    list(shinyjs::hide("placeholderTDM"))
    )
    shinyjs::show("tdm")
    shinyjs::show("logDownload")
  })
  
  #For the TDM Media, in tab Media analysis
  observeEvent(input$tdmMediaButton, {
    shinyjs::hide("iconTDMMediaEmpty")
    shinyjs::show("iconTDMMediaLoad")
    output$tdmMedia <- renderDataTable({
      get_TDMMedia()
    },
    options = list(pageLength = 10, scrollX = TRUE),
    list(shinyjs::hide("placeholderTDMMedia"))
    )
    shinyjs::show("tdmMedia")
    shinyjs::show("logMediaDownload")
  })
  
  #Event handler for the button of the matrix in tab matrix
  observeEvent(input$plotButtonMatrix, {
    shinyjs::show("plot")
    shinyjs::hide("iconPlotEmpty")
    shinyjs::show("iconPlotLoad")
    shinyjs::show("scoreBox")
    output$table.plot <- renderRHandsontable({
      rhandsontable(get_plotMatrix(), rowHeaders = TRUE) %>%
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
  
  #Eventhandler for below the materiality matrix to the manage matrix
  observeEvent(input$table.plot, {
    shinyjs::show("plot")
    shinyjs::hide("placeholderPlot")
    
    output$plot <- renderPlotly({
      weightSource <- c(input$weightPeers, input$weightInternal, input$weightNews, input$weightTwitter, input$weightReddit)
      generate_plotMatrix(input$table.plot, 20, input$X_dimension, input$Y_dimension, input$dimensionReduction, weightSource)
    })
  })
  
  #Below there is some set of wrappers to react to events in the app.
  #A set of wrappers to get the input from the inputFile commands in ui.R
  #Better solution would be to give stemming as an argument to read_documents, did not figure out how yet.
  read_documents <- reactive({
    print(input$pdfs)
    load_documents(input$pdfs, stemming = FALSE)
  })
  
  read_stemmedDocuments <- reactive({
    load_documents(input$pdfs, stemming = TRUE)
  })
  
  read_media <- reactive({
    if (is.null(input$media)){
      load_documents('media', stemming = FALSE) #provide other argument to function to read media
    }
    else {
      load_documents(input$media, stemming = FALSE)
    }
  })
  
  read_stemmedMedia <- reactive ({
    if (is.null(input$media)) {
      load_documents('media', stemming = TRUE)
    }
    else {
      load_documents(input$media, stemming = TRUE)
    }
  })
  
  read_longlists <- reactive({
    load_longlist(input$longlists)
  })
  
  #Set of wrappers to prepare/cleam word cloud input data
  wordCloudPeers <- reactive({
    prepare_wordCloud(read_documents(), 'peers')
  })
  
  wordCloudNews <- reactive({
    prepare_wordCloud(read_media(), 'news')
  })
  
  wordCloudTwitter <- reactive({
    prepare_wordCloud(read_media(), 'twitter')
  })
  
  wordCloudReddit <- reactive({
    prepare_wordCloud(read_media(), 'reddit')
  })
  
  wordCloudLonglist <- reactive({
    prepare_wordCloudLonglist(get_TDM(), input$longlistoption)
  })
  
  #Set of wrappers to plot a word cloud
  print_wordCloudPeers <- reactive({
    generate_wordCloud(wordCloudPeers(), input$wordCloudPeersNumber)
  })
  
  print_wordCloudNews <- reactive({
    generate_wordCloud(wordCloudNews(), input$wordCloudNewsNumber)
  })
  
  print_wordCloudTwitter<- reactive({
    generate_wordCloud(wordCloudTwitter(), input$wordCloudTwitterNumber)
  })
  
  print_wordCloudReddit<- reactive({
    generate_wordCloud(wordCloudReddit(), input$wordCloudRedditNumber)
  })
  
  print_wordCloudLonglist <- reactive({
    generate_wordCloud(wordCloudLonglist(), input$wordCloudLonglistNumber)
  })
  
  # aanmaken term document matrix # ralph
  # CreateTDM is messy
  get_TDM <- reactive({
    create_TDM(read_stemmedDocuments(), read_longlists(), input$scoring, input$threshold, input$longlistoption)
  })
  
  get_TDMMedia <- reactive({
    create_TDM(read_stemmedMedia(), read_longlists(), input$scoring, input$threshold, input$longlistoption)
  })
  
  get_plotMatrix <- reactive({
    prepare_plotMatrix(get_TDM(), get_TDMMedia())
  })
  
  #I don't know why this is there twice, let's leave it anyway
  print_plot <- reactive({
    weightSource <- c(input$weightPeers, input$weightInternal, input$weightNews, input$weightTwitter, input$weightReddit)
    generate_plotMatrix(input$table.plot, 20, input$X_dimension, input$Y_dimension, input$dimensionReduction, weightSource)
  })
})