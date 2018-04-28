library(pdftools) # Text Extraction


#' Reads a pdf at the specified filepath and converts it to a character data type
#'
#' @param filepath
read_pdf_to_text <- function(filepath) {
    # set temp dummy filepath
    text = pdf_text(filepath) # load text from a pdf located at the specified filepath

    return(text)
}



