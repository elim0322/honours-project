changes <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        infile <- load.dir()
    }
    if (!grepl("anns.html$", infile))
        stop("infile is not an anns.html file")
    
    src <- readLines(infile)
    editor <- readLines("test-changes.txt")
    
    ######################## "test-changes.txt" bits #######################
    # editor.start: start lines of "editor" chunks in editor
    # editor.end : end lines of "editor" chunks in editor
    editor.start <- grep("^editor editor[0-9].+$", editor)
    
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
    
    # A list of indices for the lines editted (unwanted/unmodified lines discarded).
    editor.chunks <- editor.chunks[-which.chunks]
    
    ####################### "Attempt.edit.html" bits #######################
    # Find "contenteditable" <p> tags, then remove everything except <p ... >
    #  tag as we want to retain these <p> tags.
    # ASSUMES that there is AT LEAST 1 LINE between <p> and </p>
    src.start <- grep('contenteditable=\"true\"', src)
    # Remove everything except </p> and insert a tab 
    src[src.start] <- gsub("(^<p\\s*.+>).+$", "\\1", src[src.start])
    
    # Find </p> tags, then remove everything except </p>
    endTags <- grep('</p>', src)
    
    # There could be more </p> tags than <p contenteditable="true"> tags.
    # So I'm subsetting for </p> tags that are after the <p content...>
    #  tags and finding the smallest(min) </p> tags to ensure that there
    #  are no other </p> tags between them.
    src.end <- vector("numeric", length = length(src.start))
    for (i in 1:length(src.start)) {
        src.end[i] <- min(endTags[src.start[i] < endTags])
        # Use regexpr to get the length of any empty spaces
        #  from start up to "<p.."
        temp <- regexpr("<p", src[src.start[i]])
        # Take the "match.length" attribute.
        temp <- attr(temp, "match.length")
        # Generate appropriate number of spaces.
        temp <- paste(rep(" ", temp), collapse = "")
        src[src.end] <- gsub("^.*(</p>$)",
                                paste(temp, "\\1", sep = ""),
                                src[src.end[i]])
    }
    # The lines to be edited are lines after <p...> and before </p>.
    src.chunks <- mapply(FUN = seq, src.start+1, src.end-1, SIMPLIFY = FALSE)
    # A list of indices for the lines to be editted.
    src.chunks <- src.chunks[-which.chunks]
    
    ####################### Remove unnecessary lines #######################
    # Replace each "editor lines" with a comment.
    editor <- gsub("^editor editor[0-9].*$", 
                   "<!-- This paragraph has been edited -->", editor)
    
    # Check if the lengths are equal:
    if (length(editor.chunks) != length(src.chunks)) {
        stop("Number of new content does not equal number of old content")
    }

    # Go BACKWARDS through file, replacing last chunk first
    # (so that indices for lines-to-change remain valid)
    for (i in length(editor.chunks):1) {
        oldLines <- src.chunks[[i]]
        newLines <- editor.chunks[[i]]
        firstOldLine <- src.chunks[[i]][1]
        # Rip out old lines and append new lines in the old place
        src <- append(src[-oldLines], editor[newLines], firstOldLine - 1)
    }
    
    if (is.null(outfile)) {
        outfile <- gsub("anns.html", "save.html", infile)
    }
    writeLines(src, outfile)
}
