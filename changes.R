changes <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        infile <- load.dir()
    }
    if (!grepl("save.html$", infile))
        stop("infile is not a save.html file")
    
    src <- readLines(infile)
    editor <- readLines("test-changes-01.txt")
    
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
    src.start <- grep('contenteditable=\"true\"', src)
    src[src.start] <- gsub("(^<p\\s*.+>).+$", "\\1", src[src.start])
    # Find </p> tags, then remove everything except </p>
    endTags <- grep('</p>', src)
    src[endTags] <- gsub("^.+(</p>$)", "\\1", src[endTags])
    
    src.end <- vector("numeric", length(src.start))
    # There could be more </p> tags than <p contenteditable="true"> tags.
    # So I'm subsetting for </p> tags that are after the <p content...>
    #  tags and finding the smallest(min) </p> tags to ensure that there
    #  are no other </p> tags between them.
    for (i in 1:length(src.start)) {
        src.end[i] <- min(endTags[src.start[i] < endTags])
    }
    # The lines to be edited are lines after <p...> and before </p>.
    src.chunks <- mapply(FUN = seq, src.start+1, src.end-1, SIMPLIFY = FALSE)
    # A list of indices for the lines to be editted.
    src.chunks <- src.chunks[-which.chunks]
    
    ####################### Remove unnecessary lines #######################
    # Replace each "editor lines" with a comment.
    editor <- gsub("^editor editor[0-9].*$", 
                   "<!-- This paragraph has been edited -->", editor)
    
    #### PAUL's PLAN ####
    # For each editable content ...
    # Use gsub() to remove everything EXCEPT the <p ...> tag at the
    # start of the first line of editable content (or whatever tag is
    # at the start of the first line of editable content
    # [e.g., could be a <div>])
    # Add that <p ...> in front of the new 'editor' value
    # Add </p> at the end of the new 'editor' value
    # Replace old 'src' value with new 'editor' value
    
    # Check if the lengths are equal:
    if (length(editor.chunks) == length(src.chunks)) {
        # Recycling rule and other rules prevent me from replacing a vector with
        #  another vector when they are of unequal length. So I need to make sure
        #  that the length is equal by adding lines to the one with smaller length.
        for (i in 1:length(editor.chunks)) {
            dif <- length(src.chunks[[i]]) - length(editor.chunks[[i]])
            if (dif != 0) {
                if (dif > 0) {
                    # Here length(src) is greater than length(editor), so need to add
                    #  lines in editor to match the length, then replace.
                    src[src.chunks[[i]]] <- c(editor[editor.chunks[[i]]], 
                                              rep("#ERIC'S@SPECIAL@MARKER@LINES#", temp5))
                } else if (dif < 0) {
                    # Here length(editor) > length(src), so need to add lines in src
                    #  and do the same as above.                
                    src <- append(src, rep("#ERIC'S@SPECIAL@MARKER@LINES#", abs(temp5)),
                                  # The marker line is added after the last line
                                  after = src.chunks[[i]][length(src.chunks[[i]])])
                    # The i_th src.chunks needs to be updated so I am combining a
                    #  sequence from the last element of the i_th src.chunks up to
                    #  that + how many lines are added.
                    src.chunks[[i]] <- 
                        c(src.chunks[[i]], 
                          # Seq from last element + 1 to that + temp5.
                          seq(src.chunks[[i]][length(src.chunks[[i]])] + 1,
                              src.chunks[[i]][length(src.chunks[[i]])] + abs(temp5)))
                    
                    src[src.chunks[[i]]] <- editor[editor.chunks[[i]]]
                    # Also update all the elements from the i+1_th src.chunks if
                    #  i+1 subscript is not out of bounds (the check below).
                    # By adding abs(temp5) amount to all elements in src.chunks
                    #  from i+1_th src.chunks to account for the added lines.
                    for (j in (i+1):length(editor.chunks)) {
                        src.chunks[[j]] <- src.chunks[[j]] + abs(temp5)
                    }
                }
            } else if (dif == 0) {
                src[src.chunks[[i]]] <- editor[editor.chunks[[i]]]
            }
        }
    }
    
    # Remove special marker lines if any present in src.
    if (any(grepl("#ERIC'S@SPECIAL@MARKER@LINES#", src))) {
        src <- src[-c(grep("#ERIC'S@SPECIAL@MARKER@LINES#", src))]
    }
    
    
    if (is.null(outfile)) {
        outfile <- infile
    }
    writeLines(src, outfile)
}
