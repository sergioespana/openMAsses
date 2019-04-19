#
# >> preparePlotTDM <<
# Input: term document matrix
# Output: matrix with topics, external score, internal score and show columns
#
# Note: Rename the tdm here, it is super confusing wiht the other tdm

prepare_plotMatrix <- function(tdm, tdmMedia){
  
  print(tdmMedia)
  #temp hard override, remove when tdm generator is fixed
  colnames(tdmMedia)<- c('topic', 'score', 'news','twitter','reddit')
  
  #make also automatic detector
  NumberOfSources <- 4
  
  print(tdm)
  print(tdmMedia)
  
  # Only take the topics and the score column
  tdm <- tdm[,1:2]
  tdm$Internal <- rep(0,nrow(tdm))
  tdm$News <- tdmMedia[,'news']
  tdm$Twitter <- tdmMedia[,'twitter']
  tdm$Reddit <- tdmMedia[,'reddit']
  
  # The score column is for the external axis
  colnames(tdm)[2] <- 'Peer reports'
  
  tdm[,2:(NumberOfSources+2)] <- sapply(tdm[,2:(NumberOfSources+2)], as.numeric)
  # tdm[,4] <- sapply(tdm[,4], as.numeric)
  # tdm[,5] <- sapply(tdm[,5], as.numeric)
  # tdm[,6] <- sapply(tdm[,6], as.numeric)

  # Order the tdm on the external axis score
  #tdm <- tdm[order(tdm$External, decreasing=TRUE), ]
  
  # Add a column to store whether to show a topic in the matrix or not -> default is FALSE
  tdm$Show <- rep(FALSE,nrow(tdm))
  
  #Normalize the values to a range between 10 and 0. Highest score gets a 10
  highestvalues <- as.vector(sapply(tdm[,2:(NumberOfSources+2)], max))
  
  #Set divisions by 0 to 0
  highestvalues[highestvalues == 0] <- 1
  
  #Divide all columns by highest value of respective column
  tdm[,2:(NumberOfSources+2)] <- sweep(tdm[,2:(NumberOfSources+2)], 2, highestvalues, '/')
  tdm[,2:(NumberOfSources+2)] <- apply(tdm[,2:(NumberOfSources+2)], c(1,2), function(x) x*10)
  
  #Only set topics that have a score higher than 1 to show on the matrix
  print(tdm[,2:(NumberOfSources+2)])
  show_column <- apply(tdm[,2:(NumberOfSources+2)], 1, function(x) any(x > 0))
  print(show_column)
  
  #Set final column always to show_column
  tdm[,NumberOfSources+3] <- show_column
  
  print(tdm)
  
  return(tdm)
}

#
# >> generatePlot <<
# Input: term frequency list and number to display
# Output: plot
#

generate_plotMatrix <- function(tdm, number, X_dimension, Y_dimension, dimensionreduction, weights_source){
  # Translate the table in the user interface to a R data frame
  tdm <- hot_to_r(tdm)
  
  # Add a column to the tdm
  tdm$Synonym <- NULL
  
  # Iterate over each topic in the tdm and store the first synonym in the newly created column
  for (row in 1:nrow(tdm)) {
    
    split <- strsplit(toString(tdm[row, 1]), " / ")
    split = sapply(strsplit(split[[1]], ";", fixed = TRUE), tail, 1)
    tdm$Synonym[row] <- split
  }
  
  #get the location of the column 'Show'
  show_column <- grep('Show', colnames(tdm))
  print(tdm)

  X_dimension <- as.integer(X_dimension) + 1
  Y_dimension <- as.integer(Y_dimension) + 1
  
  x_vector <- tdm[tdm[,show_column] == TRUE, X_dimension]
  y_vector <- tdm[tdm[,show_column] == TRUE, Y_dimension]
  
  ### PCA does not work, we need to come up with a new plan
  #If dimensionreduction is manual and there are more than 2 dimensions
  #per axes, we need to do some reduciton
  #Extract the X and Y vector from plot_ly to do magic
  if (dimensionreduction == 1) {
    print('automatic reduction')
    
    if (length(X_dimension) > 1) {
      x_vector <- rowMeans(tdm[tdm[,show_column] == TRUE,X_dimension])
    }
    if (length(Y_dimension) > 1) {
      y_vector <- rowMeans(tdm[tdm[,show_column] == TRUE,Y_dimension])
    }
    print(x_vector)
    print(y_vector)
  }
  
  if (dimensionreduction == 2) {
    print('manual reduction')
    tdm[,2:4] <- sweep(tdm[,2:4],2, weights_source,'*')
    
    if (length(X_dimension) > 1) {
      x_vector <- rowMeans(tdm[tdm[,show_column] == TRUE, X_dimension])
    }
    else {
      x_vector <- tdm[tdm[,show_column] == TRUE, X_dimension]
    }
    if (length(Y_dimension) > 1) {
      y_vector <- rowMeans(tdm[tdm[,show_column] == TRUE, Y_dimension])
    }
    else {
      y_vector <- tdm[tdm[,show_column] == TRUE, Y_dimension]
    }
    
    ### This code normalizes the matrix towards 10, which means the highest
    ### score receives 10,10 if the score is higher than 10.
    #Normalize the values to a range between 10 and 0. Highest score gets a 10
    highestvalue_x <- max(x_vector)
    highestvalue_y <- max(y_vector)

    #Set divisions by 0 to 1 only happens when everyting is 0. 
    #So prevents breaking
    highestvalue_x[highestvalue_x == 0] <- 1
    highestvalue_y[highestvalue_y == 0] <- 1
  
    ### As an alternative, remove the if statements to always normalize to 10.
    #Divide all columns by highest value of respective column
    if (any(x_vector > 10)) {
      x_vector <- x_vector/highestvalue_x * 10
    }
    if (any(y_vector > 10)) {
      y_vector <- y_vector/highestvalue_y * 10
    }
    print(x_vector)
    print(y_vector)
  }
  
  
  # Create a scatter plot for the tdm
  # + 1 to dimension because first column is the longlist term
  # Use only the first synonym of a topic to show in the plot, but show all synoynms of a topic when the users hovers over it
  plot <- plot_ly(x = x_vector, y = y_vector, type = 'scatter', mode = 'markers', text = tdm[tdm[,show_column] == TRUE,which(colnames(tdm) == 'Synonym')], hovertext = tdm[tdm[,show_column] == TRUE,1],
                  hoverinfo = 'x+y+text',  showlegend = FALSE, marker = list(size = 10,
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
# >> saveTDM <<
# Input: tdm, path and longlist (for topic category names)
# Output: save excel file to path
#

save_TDM <- function(tdm, path, longlist){
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