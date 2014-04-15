nspace <- function(x) {
    reg <- regexpr("<p", x)
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
    # Find "contenteditable" <p> tags and all </p> tags.
    src.start <- grep('contenteditable=\"true\"', src)
    endTags <- grep("^.*</p>.*$", src)
    
    # Find which lines are to be edited.
    src.start <- src.start[-which.chunks]
    
    # Find which element in src.start is equal to which element in endTags.
    # This is finding which line has both <p> and </p> (ie no line between).
    same.lines <- src.start[which(src.start %in% endTags)]
    if (length(same.lines) > 0) {
        # Insert an "empty line" and a "</p>" line after each line with both 
        #  <p> and </p> tags.
        for (i in length(same.lines):1) {
            s <- nspace(src[same.lines[i]])
            src <- append(src, c("empty line", paste(s, "</p>", sep="")),
                          after = same.lines[i])
        }
    }
    
    # Find which lines contain <p> and </p> tags with no lines in between.
    src.start <- grep('contenteditable=\"true\"', src)
    src.start <- src.start[-which.chunks]
    endTags <- grep("^.*</p>.*$", src)
    next.lines <- src.start[which(src.start %in% (endTags-1))]
    if (length(next.lines) > 0) {
        for (i in length(next.lines):1) {
            src <- append(src, "empty line", after = next.lines[i])
        }
    }
    
    # There could be more </p> tags than <p contenteditable="true"> tags.
    # So I'm subsetting for </p> tags that are after the <p content...>
    #  tags and finding the smallest(min) </p> tags to ensure that there
    #  are no other </p> tags between them.
    endTags <- grep("^.*<.p>.*$", src)
    src.end <- vector("numeric", length = length(src.start))
    for (i in 1:length(src.start)) {
        src.end[i] <- min(endTags[src.start[i] < endTags])
    }
    
    # Remove everything except <p ...> and </p>.
    src.start <- grep('contenteditable=\"true\"', src)
    src.start <- src.start[-which.chunks]
    for (i in 1:length(src.start)) {
        s <- nspace(src[src.start[i]])
        src[src.start] <- gsub("(.+?<p.+?>).+$", "\\1", src[src.start[i]])
        src[src.end] <- gsub("^.*(</p>.*$)", paste(s, "\\1", sep=""),
                             src[src.end[i]])
    }
    
    # The lines to be edited are lines after <p...> and before </p>.
    src.chunks <- mapply(FUN = seq, src.start+1, src.end-1, SIMPLIFY = FALSE)
    
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
