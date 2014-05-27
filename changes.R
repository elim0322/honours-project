library(XML)
library(RCurl)

changes <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        infile <- load.dir()
    }
    if (!grepl("anns.html$", infile))
        stop("infile is not an anns.html file")
    
    src <- readLines(infile)
    editor <- getURL("http://stat220.stat.auckland.ac.nz/cke/test-changes.txt",
                     userpwd="cke:cke")
    # getURL() loads "test-changes.txt" in one long vector so need to split by
    #  the carriage return, "\n"
    editor <- unlist(strsplit(editor, "\\n"))
    
    ###########################################################################
    ############################### Editor bits ###############################
    ###########################################################################
    # editor.start: start lines of "editor" chunks in editor
    # editor.end : end lines of "editor" chunks in editor
    editor.start <- grep("^EDITOR\\s.+", editor)
    
    # -1 because each end line is a line before the next start line.
    editor.end <- editor.start - 1
    # ...except the first one (because start[1]-1 is meaningless)
    editor.end <- editor.end[-1]
    # The last end line has to be manually combined into editor.end.
    editor.end <- c(editor.end, length(editor))
    
    # Each element in this list represents the lines corresponding to
    # each "editor" chunk (I separated each chunk into each element of the list).
    editor.chunks <- mapply(seq, editor.start, editor.end - 1, SIMPLIFY = FALSE)
    # Which of the "editor..." lines contain "NOT MODIFIED"?
    # Return a number to indicate which editor chunk is modified.
    which.chunks <- grep("NOT MODIFIED", editor[editor.start])
        
    ###########################################################################
    ############################## source bits ################################
    ###########################################################################
    ##### Generate appropriate closing tags
    # Only keep "<tag " and delete all attributes, etc. (can't find a way to 
    #  just select "<tag" without the empty space). This will produce "<tag ".
    src.start <- grep('contenteditable=\"true\"', src)
    endTags <- gsub("(^.*<)(.+?)\\s.+$", "\\1/\\2>", src[src.start])
    # Get rid of any white spaces before and in front of the end tags.
    endTags <- gsub("\\s*", "", endTags)

    ##### Generate src.end
    html <- htmlParse(infile)
    src.end <- vector("numeric", length = length(src.start))
    for (i in 1:length(src.start)) {
        # Get all top level nodes except save button and submit button.
        # We should not skip <div class="chunk"> nodes (because closing tags
        #  can be just before those <div> nodes).
        node <- getNodeSet(html, '//body/*[not(@id="savebutton")
                                  and not(@id="submitbutton")]')
        # Get line numbers of those top level nodes
        nxt <- sapply(node, getLineNumber)
        # Subset those greater or equal to src.start as closing tags can be in
        #  one line with opening tags
        nxtn <- nxt[nxt >= src.start[i]]
        
        # Search for closing tags
        if (length(nxtn) == 1) {
            # For last elements. There can be no more element 
            #  after it so nxtn has only one element.
            index <- grep(endTags[i], src[nxtn:length(src)])
            nxtnLines <- seq(nxtn, length(src))
            src.end[i] <- nxtnLines[index]
        } else {
            # If nxtn finds many other elements after the starting tag,
            #  just choose the first (in case for the closing tag in the same
            #  line) and second (starting line of the next node).
            nxtn2 <- nxtn[c(1,2)]
            nxtnLines <- seq(nxtn2[1], nxtn2[2])
            index <- grep(endTags[i], src[nxtnLines])
            src.end[i] <- max(nxtnLines[index])
        }
    }
    ##### Generate a list of sequence from src.start to src.end.
    # Each element of the list is a chunk.
    src.chunks <- mapply(FUN = seq, src.start, src.end, SIMPLIFY = FALSE)
    
    ##### Find which src lines are actually to be edited.
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
    
    ###########################################################################
    #################### Replace old lines with new lines #####################
    ###########################################################################
    ##### Get rid of "NOT MODIFIED" lines.
    editor.chunks <- editor.chunks[-which.chunks]
    
    ##### Check if the lengths are equal.
    if (length(editor.chunks) != length(src.chunks)) {
        stop("Number of new content does not equal number of old content")
    }
    
    ##### Remove everything except <tag ...>
    # ASSUMES only whitespace in front of <tag>
    new.src <- vector("numeric", length = length(src.start))
    for (i in 1:length(src.start)) {
        # Match for first "<" and ">" (i.e. opening tags), and
        #  remove the rest.
        new.src[i] <- gsub("(.*?<.+?>).+$", "\\1", src[src.start[i]])
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
        end.tag <- gsub("(^.*<)(.+?)\\s.+$", "\\1/\\2>", new.src[i])
        
        # Rip out old lines and append new lines in the old place
        src <- append(src[-oldLines],
                      # Subset: get rid of the first lines in editor
                      #  (e.g. EDITOR Editor-1) AND the last lines which
                      #  are just empty spaces.
                      c(new.src[i], editor[newLines[-1]], end.tag),
                      firstOldLine - 1)
    }
    
    if (is.null(outfile)) {
        outfile <- gsub("anns.html", "save.html", infile)
    }
    
    writeLines(src, outfile)
}
