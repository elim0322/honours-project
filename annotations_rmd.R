library(jsonlite)
library(XML)
library(RCurl)
# The R script "choose_file.R" contains choose.file() that
# executes file.choose() and setwd() when infile is not specified.
source("choose_file.R")

# .edit-RMD.html -> .anns-RMD.html
# To merge annotations as highlighted text.
annotations_rmd <- function(infile = NULL, outfile = NULL, annfile = NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
    if (!grepl("edit-RMD.html$", infile)) {
        stop("infile must be a edit.RMD.html file")
    }
    
    #-------------------------------------------------------------------------#
    #------------ Extract information from "test-annotations.txt" ------------#
    #-------------------------------------------------------------------------#
    if (is.null(annfile)) {
        temp <- getURL(paste0("http://stat220.stat.auckland.ac.nz/cke/",
                              "test-annotations.txt"), userpwd="cke:cke")
    } else {
        temp <- readLines(annfile)
    }
    
    # Load temp in the JavaScript Object Notation (JSON).    
    json <- fromJSON(temp, simplifyVector = FALSE)
    # Each ctrl+drag annotation is counted as a single annotation.
    numAnn <- length(json)      # numAnn = number of annotations
    
    # Create a copy of ".edit-RMDL.html" and save it as ".save-RMD.html"
    # so writeLines and readLines can be done in loops.
    if (is.null(outfile)) {
        outfile <- gsub("edit-RMD.html","anns-RMD.html", infile)
    }
    file.copy(infile, outfile, overwrite = TRUE)
    
    # Loop over each annotation.
    for (i in 1:numAnn) {
        ann <- json[[i]]
        annotation <- ann$text
        # ASSUME only one ranges value. Even with ctrl+select to select 
        # multiple annotations, we are using only the first list element.
        where <- ann$ranges[[1]]
        where.start <- where$start
        where.end <- where$end
        
        # NOTE: If multiple text are selected (by ctrl+select), they are
        #       separated by "/".
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
        
        # NOTE: The Rhtml workflow doesn't need italicising but it is used just
        #       to be consistent with the Rmd workflow.
        #       (The colour markup is ignored when reverting back from .html to
        #       so <em></em> is used for extra visibility.)
        # Generate annotation tags.
        anno.tags <- c(paste('    <p class="annotation"', 
                             'style = "background-color:coral">'), 
                       paste("    <em>Annotation: The text \"", what, 
                             "\" was annotated with the message \"", 
                             annotation, "\"</em>", sep=""),
                       "    </p>")
        # Specify XPath (R code and output in <pre>)
        xpath <- paste("/html/body//pre", where.start, 
                       "/ancestor::pre", sep = "")
        html <- htmlParse(infile)
        node <- getNodeSet(html, xpath)
        
        # xmlValues for the nodes (returns text).
        txt <- lapply(node, xmlValue)
        match <- function(t) {
            # Substring "t" from the position of where$startOffset and search 
            # for the annotated text in that.
            regexpr(annText[1], substring(t, where$startOffset), fixed = TRUE)
        }
        # Which txt has the matching pattern.
        txtMatch <- sapply(txt, match)
        # txtMatch matches for multiple entries when annotations are made on
        # the exact same words at different locations.
        # Change -1 matches (non-matches) to Inf
        txtMatch[txtMatch < 0] <- Inf
        
        # Sometimes where$startOffset values can be exactly the same, resulting
        #  in duplicated entries in txtMatch (e.g. the text "term" or "next" in
        #  "Attempt.edit.html")
        # NOTE: There is a limitation with annotator.js that its location of
        #       the annotation is arbitrary if the annotation is made on text
        #       that is duplicated multiple times in document.
        #       There is no robust way to find the exact location of the 
        #       annotated text if this happens..
        # If there is any "duplication" (not due to Inf), add a warning sign.
        if (length(txtMatch[txtMatch != Inf])>1) {
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
