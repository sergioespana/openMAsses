# source the scripts

source("functions/keyword_generation.R")
source("functions/parse longlist.R")

excel_path = ("C:/Users/hornr/source/repos/openmasses/tool/sustainalize/Longlist V2_small.xlsx")

# load the longlist in the same way as the shiny app would do
longlist = ozp_parse_longlist(excel_path)


 for (row in 1:nrow(longlist)) {
    extracted.description <- longlist[row, description_col_nr]
     terms.frequency <- 0
     if (extracted.description != "") {
        terms.list <- ozp_generate_keywords(longlist[row, description_col_nr])
        
    }
    
    }