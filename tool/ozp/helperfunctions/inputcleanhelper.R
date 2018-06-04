clean_text <- function(text) {

    # Replace line breaks, page breaks and tabs with a space
    text <- lapply(text, function(x) gsub("[\r\n\t]", " ", x))

    # Replace slashes
    text <- lapply(text, function(x) gsub("\u2215|\u2044", " ", x))

    # Replace parentheses with a space
    text <- lapply(text, function(x) gsub("\\(", " ", x))
    text <- lapply(text, function(x) gsub("\\)", " ", x))

    # Remove all full stops
    text <- lapply(text, function(x) gsub(".", "", x, fixed = TRUE))

    # Remove all question marks, quotation marks, asterisks, bullet points and weird symbols
    text <- lapply(text, function(x) gsub("?", "", x, fixed = TRUE))
    text <- lapply(text, function(x) gsub("\"", "", x, fixed = TRUE))
    text <- lapply(text, function(x) gsub("*", "", x, fixed = TRUE))
    text <- lapply(text, function(x) gsub("?", "", x, fixed = TRUE))
    text <- lapply(text, function(x) gsub("?", "", x, fixed = TRUE))

    # Remove all sorts of commas, full stops, colons, semicolons, question marks, exclamation points, quotation marks, euro signs, dollar signs, pound signs
    text <- lapply(text, function(x) gsub("\u002C|\uFF0C|\u0326|\u02BB|\u003A|\uFF1A|\u003B|\uFF1B|\u204F|\uFE54|\u003F|\uFF1F|\u0021|\uFF01|\u201C|\u2018|\u2019|\u201D|\u20AC|\u0024|\uFF04|\u00A3|\uFFE1", "", x))

    # Change all text to lower case
    text <- lapply(text, tolower)

    return(text)
}