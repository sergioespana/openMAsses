#
# >> preparePlotTDM <<
# Input: term document matrix
# Output: matrix with topics, external score, internal score and show columns
#

preparePlotTDM <- function(tdm, tdmMedia){
  
  print(tdm)
  print(tdmMedia)
  
  # Only take the topics and the score column
  tdm <- tdm[,1:2]
  
  # The score column is for the external axis
  colnames(tdm)[2] <- 'Peer reports'
  
  # Add the internal axis column
  tdm$Internal <- rep(0,nrow(tdm))
  tdm$News <- tdmMedia[3]
  #tdm$Social <- tdmMedia[2]
  #colnames(tdm)[5] <- 'Social media'
  
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