library(XML)
library(RCurl)
# The R script "choose_file.R" contains choose.file() that
# executes file.choose() and setwd() when infile is not specified.
source("choose_file.R")

# .anns-RMD.html -> save-RMD.html
# To merge changes made.
changes_rmd <- function(infile = NULL, outfile = NULL, changefile = NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
    if (!grepl("anns-RMD.html$", infile)) {
        stop("infile must be an anns-RMD.html file")
    }
    src <- readLines(infile)
    
    #-------------------------------------------------------------------------#
    #--------------- Fetch "test-changes.txt" from the server ----------------#
    #-------------------------------------------------------------------------#
    if (is.null(changefile)) {
        editor <- getURL(paste0("http://stat220.stat.auckland.ac.nz/cke/",
                                "test-changes.txt"), userpwd="cke:cke")
        # getURL() loads "test-changes.txt" in one long vector so need to
        # split by the carriage return, "\n"
        editor <- unlist(strsplit(editor, "\\n"))
    } else {
        editor <- readLines(changefile)
    }
    
    #-------------------------------------------------------------------------#
    #-------------------------------Editor bits ------------------------------#
    #-------------------------------------------------------------------------#
    # Start lines of each chunk of changes in editor.
    editor.start <- grep("^EDITOR\\s.+", editor)
    
    # Generate end lines of each chunk of changes in editor.
    # -1 because each end line is a line before the next start line.
    editor.end <- editor.start - 1
    editor.end <- editor.end[-1]  # begin.start[1]-1 is meaningless
    # The last end line has to be manually combined into editor.end.
    editor.end <- c(editor.end, length(editor))
    
    # A list of sequences of lines for each chunk of changes.
    editor.chunks <- mapply(seq, editor.start, editor.end-1, SIMPLIFY = FALSE)
    
    # Find chunks that are not modified.
    which.chunks <- grep("NOT MODIFIED", editor[editor.start])
    
    #-------------------------------------------------------------------------#
    #----------------------------- source bits -------------------------------#
    #-------------------------------------------------------------------------#
    ##### Generate appropriate closing tags
    # Find the lines with contenteditable="true", remove all attributes except
    # "<tag ", and then generate matching closing tags.
    src.start <- grep('contenteditable=\"true\"', src)
    endTags <- gsub("(^.*<)(.+?)\\s.+$", "\\1/\\2>", src[src.start])
    endTags <- gsub("\\s*", "", endTags)
    
    ##### Generate src.end
    html <- htmlParse(infile)
    src.end <- vector("numeric", length = length(src.start))
    for (i in 1:length(src.start)) {
        # Get all nodes (top, bottom and everything) inside this XPath.
        node <- getNodeSet(html, paste('//body/div[@class="container-fluid',
                                       'main-container"]//*'))
        # Get line numbers of those top level nodes
        nxt <- sapply(node, getLineNumber)
        # Subset those greater or equal to src.start as closing tags can be in
        #  one line with opening tags
        nxtn <- nxt[src.start[i] <= nxt]
        
        # nxtn can either be length of 1 (when it's the last element)
        # or more than one.
        if (length(nxtn) == 1) {
            # For last elements. There can be no more element 
            # after it so nxtn has only one element.
            index <- grep(endTags[i], src[nxtn:length(src)])
            nxtnLines <- seq(nxtn, length(src))
            src.end[i] <- nxtnLines[index]
        } else {
            # If nxtn finds many other elements after the starting tag, just
            # choose the first (in case for the closing tag in the same line) 
            # and second (starting line of the next node).
            nxtn2 <- nxtn[c(1,2)]
            nxtnLines <- seq(nxtn2[1], nxtn2[2])
            index <- grep(endTags[i], src[nxtnLines])
            src.end[i] <- max(nxtnLines[index])
        }
    }
    
    ##### Generate a list of sequence from src.start to src.end.
    src.chunks <- mapply(FUN = seq, src.start, src.end, SIMPLIFY = FALSE)
    
    ##### Find which src lines are actually to be edited.
    # In case ALL editable sections are being edited.
    if (length(which.chunks) > 0) {
        for (i in 1:length(which.chunks)) {
            # Find the "id" of "NOT MODIFIED" editor chunks
            notEditedLines <- unlist(editor.chunks[which.chunks[i]])
            
            # Remove some strings to get the right id (i.e. "Editor-##").
            notEditedId <- gsub("^EDITOR\\s(.+)\\sNOT MODIFIED$", "\\1",
                                editor[notEditedLines][1])
            
            # Match the id from editor to the src chunks
            notEditedIndex <- sapply(src.chunks,
                                     function(x) {
                                         grepl(notEditedId, src[x[1]])
                                     })
            
            # "notEditedIndex" is a logical vector, TRUE for the correct match
            #  so we want to subset for the FALSE ones.
            src.chunks <- src.chunks[!(notEditedIndex)]
        }
    }
    
    #-------------------------------------------------------------------------#
    #------------------- Replace old lines with new lines --------------------#
    #-------------------------------------------------------------------------#
    ##### Get rid of "NOT MODIFIED" lines.
    if (length(which.chunks) > 0) {
        editor.chunks <- editor.chunks[-which.chunks]
    }
    
    ##### Check if the lengths of source and editor bits are equal.    
    if (length(editor.chunks) != length(src.chunks)) {
        stop("Number of new content does not equal number of old content")
    }
    
    ##### Generate beginning tags for edited chunks.
    # Remove everything except <tag ...>, assuming only whitespace in front of
    # <tag> (assumes no text in front of <tag>).
    open.tag <- vector("numeric", length = length(src.start))
    for (i in 1:length(src.start)) {
        open.tag[i] <- gsub("(.*?<.+?>).+$", "\\1", src[src.start[i]])
    }
    # If there are edited chunks, extract the id attributes of "NOT MODIFIED"
    # chunk of changes in "editor", and remove opening tags matching the 
    # unedited chunks.
    if (length(which.chunks) > 0) {
        NotEditedIds <- gsub("(^EDITOR )(.+)( NOT MODIFIED$)", "\\2",
                             editor[editor.start[which.chunks]])
        for (i in 1:length(NotEditedIds)) {
            open.tag <- open.tag[-grep(NotEditedIds[i], open.tag)]
        }
    }
    
    # Go BACKWARDS through file, replacing last chunk first
    # (so that indices for lines-to-change remain valid)
    for (i in length(editor.chunks):1) {
        oldLines <- src.chunks[[i]]
        # Parse as a text.
        srcId <- htmlParse(src[oldLines], asText=TRUE)
        idNode <- getNodeSet(srcId, "//body/*[1]")
        # search for id attributes
        id <- xmlGetAttr(idNode[[1]], "id")
        # find which editor.chunks contain the matched id
        index <- sapply(editor.chunks, 
                        function(x) {
                            grepl(id, editor[x[1]])
                        })
        
        newLines <- unlist(editor.chunks[index])
        firstOldLine <- src.chunks[[i]][1]
        end.tag <- gsub("(^.*<)(.+?)\\s.+$", "\\1/\\2>", open.tag[i])
        
        # Rip out old lines and append new lines in the old place
        src <- append(src[-oldLines],
                      # Subset: get rid of the first lines in editor
                      #  (e.g. EDITOR Editor-1) AND the last lines which
                      #  are just empty spaces.
                      c(open.tag[i], editor[newLines[-1]], end.tag),
                      firstOldLine - 1)
    }
    
    if (is.null(outfile)) {
        outfile <- gsub("anns-RMD.html", "save-RMD.html", infile)
    }
    
    writeLines(src, outfile)
}
