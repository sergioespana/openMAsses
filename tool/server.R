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

source("functions/parse longlist.R")
source("functions/keyword_generation.R")
source("functions/wordclouds.R")
source("functions/tdm.R")
source("functions/matrix.R")
source("functions/data_loaders.R")


shinyServer(function(input, output) {
  
  #Creates a table of all pdf files that were uploaded, enables analysis buttons and writes name down
  #for overview
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
          enable("plotButton")
          removeClass("not-allowed", "not-allowed")
          enable("wordCloudButtonLonglist")
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
      return(table)
    }
    else {
      enable('wordCloudButtonSocial')
      enable('wordCloudButtonNews')
      removeClass('media1', 'missing')
      removeClass('media2', 'missing')
      removeClass('media3', 'missing')
      removeClass('media4', 'missing')
      if (!is.null(input$longlists)) {
        removeClass("not-allowed", "not-allowed")
        enable("tdmMediaButton")
        enable("tdmMediaDownload")
        enable("plotButton") #TODO later, this is the button for the tab matrix
      }
      removeClass("not-allowed", "not-allowed")
      if (length(input$pdfs$name) == 1) {
        colnames(table) <- "Uploaded document"
      }
      else {
        colnames(table) <- "Uploaded documents"
      }
      # list document names in the documents section of the HTML.
      for (media in 1:length(input$media$name)) {
        table[media, 1] <- input$media$name[media]
      }
      return(table)
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
      saveTDM(getTDM(), path, readLonglists())
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
  
  #A set of button event handlers
  #This one is for PDF word clouds
  observeEvent(input$wordCloudButtonPDF, {
    shinyjs::hide("iconWordCloudPDFEmpty")
    shinyjs::show("iconWordCloudPDFLoad")
    output$wordCloudPlotPDF <- renderPlot({
      printWordCloudPDF()
      shinyjs::hide("placeholderWordCloudPDF")
    })
    shinyjs::show("wordCloudPlotPDF")
  })
  
  #For News word clouds
  observeEvent(input$wordCloudButtonNews, {
    shinyjs::hide("iconWordCloudNewsEmpty")
    shinyjs::show("iconWordCloudNewsLoad")
    output$wordCloudPlotNews <- renderPlot({
      printWordCloudNews()
      shinyjs::hide("placeholderWordCloudNews")
    })
    shinyjs::show("wordCloudPlotNews")
  })
  
  #For Social media word clouds
  observeEvent(input$wordCloudButtonSocial, {
    shinyjs::hide("iconWordCloudSocialEmpty")
    shinyjs::show("iconWordCloudSocialLoad")
    output$wordCloudPlotSocial <- renderPlot({
      printWordCloudSocial()
      shinyjs::hide("placeholderWordCloudSocial")
    })
    shinyjs::show("wordCloudPlotSocial")
  })
  
  #For longlist word clouds
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
  
  #For the TDM, in tab Term Document Matrix
  observeEvent(input$tdmButton, {
    shinyjs::hide("iconTDMEmpty")
    shinyjs::show("iconTDMLoad")
    output$tdm <- renderDataTable({
      getTDM()
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
      getTDMMedia()
    },
    options = list(pageLength = 10, scrollX = TRUE),
    list(shinyjs::hide("placeholderTDMMedia"))
    )
    shinyjs::show("tdmMedia")
    shinyjs::show("logMediaDownload")
  })
  
  #Event handler for the button of the matrix in tab matrix
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
  
  #Eventhandler for below the materiality matrix to the manage matrix
  observeEvent(input$table.plot, {
      shinyjs::show("plot")
      shinyjs::hide("placeholderPlot")
      
      output$plot <- renderPlotly({
        weights_source <- c(input$weightPeers, input$weightInternal, input$weightNews)
        generatePlot(input$table.plot, 20, input$X_dimension, input$Y_dimension, input$dimensionreduction, weights_source)
      })
  })
  
  #Below there is some set of wrappers to react to events in the app.
  #A set of wrappers to get the input from the inputFile commands in ui.R
  readDocuments <- reactive({
    documentsLoad(input$pdfs)
  })
  
  readMedia <- reactive({
    print(input$media)
    loadMedia(input$media)
  })
  
  readDocumentscloud <- reactive({
      documentsLoadwordcloud(input$pdfs)
  })

  readLonglists <- reactive({
    longlistLoad(input$longlists)
  })
  
  #Set of wrappers to prepare/cleam word cloud input data
  wordCloudPDF <- reactive({
    prepareWordCloudPDF(readDocumentscloud())
  })
  
  wordCloudNews <- reactive({
    mediaNews <- readMedia()
    prepareWordCloudMedia(mediaNews)
  })
  
  wordCloudSocial <- reactive({
    mediaSocial <- readMedia()
    prepareWordCloudMedia(mediaSocial)
  })

  wordCloudLonglist <- reactive({
    prepareWordCloudLonglist(getTDM())
  })
  
  #Set of wrappers to plot a word cloud
  printWordCloudPDF <- reactive({
    generateWordCloud(wordCloudPDF(), input$wordCloudPDFNumber)
  })
  
  printWordCloudNews <- reactive({
    generateWordCloud(wordCloudNews(), input$wordCloudNewsNumber)
  })
  
  printWordCloudSocial<- reactive({
    generateWordCloud(wordCloudSocial(), input$wordCloudSocialNumber)
  })

  printWordCloudLonglist <- reactive({
    generateWordCloud(wordCloudLonglist(), input$wordCloudLonglistNumber)
  })

  # aanmaken term document matrix # ralph
  # CreateTDM is messy
  getTDM <- reactive({
    createTDM(readDocuments(), readLonglists(), input$scoring, input$threshold, input$longlistoption)
  })
  
  getTDMMedia <- reactive({
    media <- transformMedia(readMedia())
    print(media)
    createTDM(media, readLonglists(), input$scoring, input$threshold, input$longlistoption)
  })

  getPlotTDM <- reactive({
    preparePlotTDM(getTDM(), getTDMMedia())
  })
  
  printPlot <- reactive({
    weights_source <- c(input$weightPeers, input$weightInternal, input$weightNews)
    generatePlot(input$table.plot, 20, input$X_dimension, input$Y_dimension, input$dimensionreduction, weights_source)
  })
})