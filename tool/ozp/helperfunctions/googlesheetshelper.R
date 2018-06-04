# load google sheets package

library(googlesheets)
library(tidyverse)

read_google_sheet <- function() {


    # open browser to get google sheets access.
    # to do: build some kind of interactive window?
    gs_ls()

    #set sheetname
    sheetname = gs_title("LonglistExample")


    raw_google_sheet = gs_read(sheetname)

    test_frame = as.data.frame(raw_google_sheet)

    return (test_frame)
}