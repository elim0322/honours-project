library(jsonlite)
library(XML)

annotations <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        infile <- load.dir()
    }
    if (!grepl("edit.html$", infile))
        stop("infile is not an edit.html file")
    
    ########## Extract useful information from "test-annotations.txt" using json ##########
    temp <- readLines("test-annotations.txt", warn = FALSE)
    json <- fromJSON(temp, simplifyVector = FALSE)
    
    # numAnn: number of annotations
    #  (each ctrl+drag annotation is counted as a single annotation)
    numAnn <- length(json$rows)
    
    # Create a copy of "Attempt.edit.html" and save it as "Attempt.save.html"
    #  so I can write lines into "Attempt.save.html" and carry out other
    #  stuff in the for loop below:
    if (is.null(outfile)) {
        outfile <- gsub("edit.html","anns.html", infile)
    }
    file.copy(infile, outfile, overwrite = TRUE)
    
    for (i in 1:numAnn) {
        ann <- json$rows[[i]]
        who <- ann$user
        annotation <- ann$text
        # ASSUME only one ranges value
        # Even with ctrl+select annotations, we are using the first list
        #  element.
        where <- ann$ranges[[1]]
        where.start <- where$start
        where.end <- where$end
        
        # what <- ann$quote: this line became following:
        # If ann$quote contains " / ",
        if (grepl(" / ", ann$quote)) {
            temp <- unlist(strsplit(ann$quote, " / "))
            if (length(temp) == 2) {
                # If there are two words, then paste them
                #  as "word1" & "word2".
                what <- paste(temp, collapse = "\" & \"")
            } else if (length(temp) >= 3) {
                # If there are more than 2 words, then paste
                #  them as "word1", "word2", ... (temp1).
                # Then, paste temp1 with the last word as
                #  "word1", ... & "lastword"
                temp1 <- paste(temp[1:length(temp)-1], collapse = "\", \"")
                what <- paste(temp1, temp[length(temp)], collapse = "\" & \"")
            }
            # Just trying to use the correct grammar. Plural:
            anno.tags <- c('    <p class="annotation" style = "background-color:coral">', 
                           paste("    The texts \"", what, 
                                 "\" were annotated with the message \"", 
                                 annotation, "\" by \"", who, "\"", sep=""),
                           "    </p>")
        } else {
            temp <- ann$quote
            anno.tags <- c('    <p class="annotation" style = "background-color:coral">', 
                           paste("    The text \"", temp, 
                                 "\" was annotated with the message \"", 
                                 annotation, "\" by \"", who, "\"", sep=""),
                           "    </p>")
        }
        
        xpath <- paste("/html/body/div[@class='chunk']", where.start, 
                             "/ancestor::div[@class='chunk']", sep = "")
        html <- htmlParse("Attempt.anns.html")
        node <- getNodeSet(html, xpath)
        
        # xmlValues for the nodes.
        txt <- lapply(node, xmlValue)
        match <- function(t) {
            # Substring "t" from the position of where$startOffset.
            # Search for that pattern in what.
            regexpr(temp[1], substring(t, where$startOffset), fixed = TRUE)
        }
        # Which txt has the matching pattern.
        temp2 <- sapply(txt, match)
        # which(temp != -1) selects the correct node, as it will not be -1
        # (being -1 indicates the pattern not matched in txt)
        lineNumber <- getLineNumber(node[[which(temp2 != -1)]])
        src <- readLines(outfile, warn = FALSE)
        src <- append(src, anno.tags, after = lineNumber - 1)
        writeLines(src, outfile)
    }
}
