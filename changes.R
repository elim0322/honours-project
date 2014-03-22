    # Decided to use Attempt.edit.html instead of Attempt.post.html
    #  due to <p contendeditable> tags being easy to make use of.
    src <- readLines("Attempt.edit.html")
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
    # each "editor" chunk.
    editor.chunks <- mapply(seq, editor.start, editor.end, SIMPLIFY = FALSE)
    
    # Which of the "editor..." lines contain "NOT MODIFIED"?
    # Return a number to indicate which editor chunk is modified.
    temp1 <- grepl("NOT MODIFIED", editor[editor.start])
    which.chunks <- which(temp1 == FALSE)
    
    # A list of indices for the lines editted.
    editor.chunks <- editor.chunks[which.chunks]
    
    ####################### "Attempt.edit.html" bits #######################
    src.start <- grep('contenteditable=\"true\"', src)
    endTags <- grep('</p>', src)
    src.end <- vector("numeric", length(src.start))
    # There could be more </p> tags than <p contenteditable="true"> tags.
    # So I'm subsetting for </p> tags that are after the <p content...>
    #  tags and finding the smallest(min) </p> tags to ensure that there
    #  are no other </p> tags between them.
    for (i in 1:length(src.start)) {
        src.end[i] <- min(endTags[src.start[i] < endTags])
    }
    src.chunks <- mapply(FUN = seq, src.start, src.end, SIMPLIFY = FALSE)
    # A list of indices for the lines to be editted.
    src.chunks <- src.chunks[which.chunks]
    
    ####################### Remove unnecessary lines #######################
    # editor[unlist(editor.chunks[which.chunks])]
    # src[unlist(src.chunks[which.chunks])]
    
    # Replace each "editor lines" with a comment.
    editor <- gsub("^editor editor[0-9].*$", 
                   "<-- This paragraph has been editted -->", editor)
    
    # Search empty lines (in editor) and remove them.
    for (i in 1:length(editor.chunks)) {
        temp2 <- grep("^editor editor[0-9].*$", editor[editor.chunks[[i]]])
        temp3 <- which(nchar(editor[editor.chunks[[i]]]) == 0)
        editor.chunks[[i]] <- editor.chunks[[i]][-c(temp2,temp3)]
    }
    
    # Using the same idea as above, remove "<p>" tags.
    for (i in 1:length(src.chunks)) {
        temp4 <- grep('(contenteditable=\"true\")|(^.*</p>.*$)', 
                      src[src.chunks[[i]]])
        src.chunks[[i]] <- src.chunks[[i]][-c(temp4)]
    }
    
    # Check if the lengths are equal:
    if (length(editor.chunks) == length(editor.chunks)) {
    # Recycling rule and other rules prevent me from replacing a vector with
    #  another vector when they are of unequal length. So I need to make sure
    #  that the length is equal by adding lines to the one with smaller length.
        for (i in 1:length(editor.chunks)) {
            temp5 <- length(src.chunks[[i]]) - length(editor.chunks[[i]])
            if (temp5 != 0) {
                if (temp5 > 0) {
                    # Here length(src) is greater than length(editor), so need to add
                    #  lines in editor to match the length, then replace.
                    src[src.chunks[[i]]] <- c(editor[editor.chunks[[i]]], 
                                              rep("#ERIC'S@SPECIAL@MARKER@LINES#", temp5))
                    } else if (temp5 < 0) {
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
                              seq(src.chunks[[i]][length(src.chunks[[i]])] + 1,
                                  src.chunks[[i]][length(src.chunks[[i]])] + abs(temp5)))
                        
                        # Also update all the elements from the n+1_th src.chunks.
                        # I don't know if this line will work....
                        ############ Needs test ###################################
                        for (j in i:length(src.chunks)) {
                            src.chunks[[j+1]] <- src.chunks[[j+1]] + abs(temp5)
                        }
                        src[src.chunks[[i]]] <- editor[editor.chunks[[i]]]
                    }
            } else if (temp5 == 0) {
                src[src.chunks[[i]]] <- editor[editor.chunks[[i]]]
            }
        }
    }
