library(XML)
library(RCurl)

nspace <- function(x) {
    # regexpr is always non-greedy (tested). It matches for the first "<"
    #  encountered.
    # It will be safe no matter which tag is used (whether <div> or <p>)
    reg <- regexpr("<", x)
    n <- attr(reg, "match.length")
    paste(rep(" ", n), collapse = "")
}

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
    ######################### "test-changes.txt" bits #########################
    ###########################################################################
    # editor.start: start lines of "editor" chunks in editor
    # editor.end : end lines of "editor" chunks in editor
    editor.start <- grep("^editor\\s.+", editor)
    
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
    
    # A list of lines actually edited.
    editor.chunks <- editor.chunks[-which.chunks]
    
    ###########################################################################
    ######################## "Attempt.edit.html" bits #########################
    ###########################################################################
    # Find all tags that contain 'contenteditable="true"' attribute.
    src.start <- grep('contenteditable=\"true\"', src)
    
    #################### Generate appropriate closing tags ####################
    # Only keep "<tag " and delete all attributes, etc. (can't find a way to  #
    #  just select "<tag" without the empty space). This will produce "<tag ".#
    openTags <- gsub("^.*(<.+\\s+?).+$", "\\1", src[src.start])               #
    # Frist get rid of an empty space at the end for every opening tag. Then, #
    #  replace "<" with "</" to get closing tags matching the opening tags.   #
    endTags <- gsub(" ", ">", openTags)                                       #
    endTags <- gsub("<", "</", endTags)                                       #
    ###########################################################################
    
    # Find which lines are actually to be edited.
    src.start <- src.start[-which.chunks]
    # Only select the opening tags that are actually to be edited
    openTags <- openTags[-which.chunks]
    endTags <- endTags[-which.chunks]
    
    html <- htmlParse(infile)
    src.end <- vector("numeric", length = length(src.start))
    for (i in 1:length(src.start)) {
        # Get all top level nodes except save button, submit button and knitr
        #  generated stuff
        node <- getNodeSet(html, '//body/*[not(@class="chunk") and
                               not(@id="savebutton") and 
                               not(@id="submitbutton")]')
        # Get line numbers of those top level nodes
        nxt <- sapply(node, getLineNumber)
        nxtn <- nxt[nxt >= src.start[i]]

        # For last elements. There can be no more element after it so
        #  nxtn has only one element.
        if (length(nxtn) == 1) {
            index <- grep(endTags[i], src[nxtn:length(src)])
            nxtnLines <- seq(nxtn, length(src))
            src.end[i] <- nxtnLines[index]
        } else {
            # If nxtn finds many other elements after the starting tag,
            #  just choose the first (in case for the closing tag in the same
            #  line) and second (next node)
            nxtn2 <- nxtn[c(1,2)]
            nxtnLines <- seq(nxtn2[1], nxtn2[2])
            index <- grep(endTags[i], src[nxtnLines])
            src.end[i] <- min(nxtnLines[index])
        }
        # NOTE: could not follow Paul's suggestion because I can't xmlParse()
        #  can only htmlParse() because of javascripts and things...
    }

#     ###############
#     # There could be more closing tags than <tag contenteditable="true">. 
#     # So I'm subsetting for </tag> that are after the <tag contenteditable...>
#     #  tags and finding the smallest(min) closing tags to ensure that there
#     #  are no other </p> tags between them.
#     src.end <- vector("numeric", length = length(src.start))
#     for (i in 1:length(src.start)) {
#         src.end[i] <- min(endTags[src.start[i] <= endTags])
#     }
#     
#     # All top level nodes.
#     html <- htmlParse(infile)
#     node <- getNodeSet(html, '//body/*[not(@class="chunk") and 
#                        not(@id="savebutton") and not(@id="submitbutton")]')
#     topLines <- sapply(node, getLineNumber)
#     endLines <- grep(endTags, src)
#     src.end <- vector("numeric", length = length(src.start))
#     for (i in 1:length(src.start)) {
#         endLines<- grep(endTags[i], src)
#         which(endLines <= topLines & endLines >= src.start[i])
# 
#     }
    
    # Remove everything except <tag ...>
    # ASSUMES only whitespace in front of <tag>
    new.src <- vector("numeric", length = length(src.start))
    for (i in 1:length(src.start)) {
        # Match for first "<" and ">" (i.e. opening tags), and
        #  remove the rest.
        new.src[i] <- gsub("(.*?<.+?>).+$", "\\1", src[src.start[i]])
    }
    
    # The lines to be edited are lines after <tag...> and before </tag>.
    src.chunks <- mapply(FUN = seq, src.start, src.end, SIMPLIFY = FALSE)
    
#     ######################## Remove unnecessary lines #########################
#     # Replace each "editor lines" with a comment.
#     editor <- gsub("^editor editor[0-9].*$", 
#                    "<!-- This paragraph has been edited -->", editor)
    
    # Check if the lengths are equal:
    if (length(editor.chunks) != length(src.chunks)) {
        stop("Number of new content does not equal number of old content")
    }

    # Go BACKWARDS through file, replacing last chunk first
    # (so that indices for lines-to-change remain valid)
    for (i in length(editor.chunks):1) {
        oldLines <- src.chunks[[i]]
        # lines to search for the id of src.chunks[[i]] then insert appropriate
        #  chunks.
        src.id.lines = src.chunks[[i]][1]
        # Searching for id="something", searching for that something
        id = gsub("^.+id\\s*=\\s*\"(.+).?\".+$", "\\1", src[src.id.lines])
        
        ###### need to find which element of editor.chunks contain id
        # then assign it to newLines
        l = grep(id, editor.chunks[[i]][1])
        
        l = grep(id, editor[unlist(editor.chunks)])
        
        newLines <- editor.chunks[[i]]
        
        firstOldLine <- src.chunks[[i]][1]
        
        # Rip out old lines and append new lines in the old place
        src <- append(src[-oldLines],
                      c(new.src[i], editor[newLines],
                        # Try to match indenting
                        paste0(nspace(new.src[i]), endTags[i])),
                      firstOldLine - 1)
    }
    
    if (is.null(outfile)) {
        outfile <- gsub("anns.html", "save.html", infile)
    }
    writeLines(src, outfile)
}
