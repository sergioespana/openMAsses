require(readxl)
require(tidyverse)

#' Parse longlist
#'
#' @param a data object from read_xlsx function that is based on the longlist structure.
#' @description This function parses the longlist excel to a machine readable input. it removes columns that are NA and it unites all descriptions and topics into one
#' 
#'
#' @return  a tidyverse tibble that represents the machine readable longlist Topics are grouped in the 'Topics' column whereas Descriptions are grouped in the 'Descriptions'column
#' @export
#'
#' @examples
ozp_parse_longlist <- function(input) {

    
   
    input = read_xlsx(input)

    data = as.tibble(input)

    # filter out columns that have all NA values
    data = Filter(function(x)!all(is.na(x)), data)

    # set all NA values to blank
    data[is.na(data)] <- ""

    # use grep to search the column names for all the possible descriptions
    column_names = grep("Description", names(data), value = TRUE)

    # unite all the descriptions into one column seperated by ;
    output = unite(data, "Descriptions", column_names, sep = ';', remove = TRUE)

    # use grep to search the column names for all the possible topics
    column_names = grep("Topic", names(data), value = TRUE)
    # unite all the Topics into one column seperated by ;
    output = unite(output, "Topics", column_names, sep = ';', remove = TRUE)

    # remove all the trailing ; this makes the data inconsistent so there are no empty topics or descriptions later on
    output = sapply(output, function(x) {gsub(pattern = ";{1,10}$",replacement = "",x) })

    return(output)
}
