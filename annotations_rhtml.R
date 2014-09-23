library(jsonlite)
library(XML)
library(RCurl)
library(httr)
# The R script "choose_file.R" contains choose.file() that
# executes file.choose() and setwd() when infile is not specified.
source("choose_file.R")

# .edit-RHTML.html -> .anns-RHTML.html
annotations_rhtml <- function(infile = NULL, outfile = NULL, annfile = NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
    if (!grepl("edit-RHTML.html$", infile)) {
        stop("infile must be a edit.RHTML.html file")
    }
    
    ############# Extract information from "test-annotations.txt" #############
    if (is.null(annfile)) {
        temp <- getURL(paste0("http://stat220.stat.auckland.ac.nz/cke/",
                              "test-annotations.txt"), userpwd="cke:cke")
    } else {
        temp <- readLines(annfile)
    }
    
    # Load temp in JSON.
    json <- fromJSON(temp, simplifyVector = FALSE)
    # Each ctrl+drag annotation is counted as a single annotation.
    numAnn <- length(json)      # numAnn = number of annotations
    
    # Create a copy of ".edit-RHTML.html" and save it as ".save-RHTML.html"
    # so writeLines and readLines can be done in loops.
    if (is.null(outfile)) {
        outfile <- gsub("edit-RHTML.html","anns-RHTML.html", infile)
    }
    file.copy(infile, outfile, overwrite = TRUE)
    
    for (i in 1:numAnn) {
        ann <- json[[i]]
        annotation <- ann$text
        # ASSUME only one ranges value. Even with ctrl+select to select 
        # multiple annotations, we are using only the first list element.
        where <- ann$ranges[[1]]
        where.start <- where$start
        where.end <- where$end
        
        # what <- ann$quote: this line became following:
        # If ann$quote contains " / ",
        if (grepl(" / ", ann$quote)) {
            annText <- unlist(strsplit(ann$quote, " / "))
            numText <- length(annText)
            if (numText == 2) {
                # If there are two words, then paste them
                #  as "word1" & "word2".
                what <- paste(annText, collapse = "\" & \"")
            } else if (numText >= 3) {
                # If there are more than 2 words, then paste
                #  them as "word1", "word2", ... (temp).
                # Then, paste temp with the last word as
                #  "word1", ... & "lastword"
                temp <- paste(annText[-numText], collapse = "\", \"")
                what <- paste(temp, annText[numText], collapse = "\" & \"")
            }
        } else {
            annText <- what <- ann$quote
        }
        # Generate annotation tags.
        anno.tags <- c(paste('    <p class="annotation"', 
                             'style = "background-color:coral">'), 
                       paste("    The text \"", what, 
                             "\" was annotated with the message \"", 
                             annotation, "\"", sep=""),
                       "    </p>")
        # Specify XPath.
        xpath <- paste("/html/body/div[@class='chunk']", where.start, 
                       "/ancestor::div[@class='chunk']", sep = "")
        html <- htmlParse(infile)
        node <- getNodeSet(html, xpath)
        
        # xmlValues for the nodes.
        txt <- lapply(node, xmlValue)
        match <- function(t) {
            # Substring "t" from the position of where$startOffset.
            # Search for that pattern in what.
            regexpr(annText[1], substring(t, where$startOffset), fixed = TRUE)
        }
        # Which txt has the matching pattern.
        txtMatch <- sapply(txt, match)
        # txtMatch matches for multiple entries when annotations are made on
        #  same word at different locations
        # Change -1 matches to Inf
        txtMatch[txtMatch < 0] <- Inf
        
        # Sometimes where$startOffset values can be exactly the same, resulting
        #  in duplicated entries in txtMatch (e.g. the text "term" or "next" in
        #  "Attempt.edit.html")
        # If there is any "duplication" (not due to Inf), add a warning sign.
        if (any(duplicated(txtMatch[txtMatch != Inf]))) {
            anno.tags <- append(anno.tags,
                                '<br><font color="brown"> Warning: annotated
                                text may not be the correct one. There may be
                                duplicates.</font>',
                                after = length(anno.tags)-1)
        }
        whichMatch <- which.min(txtMatch)
        
        lineNumber <- sapply(node[whichMatch], getLineNumber)
        
        src <- readLines(outfile, warn = FALSE)
        src <- append(src, anno.tags, after = lineNumber - 1)
        writeLines(src, outfile)
    }
}