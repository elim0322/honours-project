changes <- function(infile) {
    src <- readLines(infile)
    chgs <- readLines("changes.txt")
    
    # The editted lines are after the "editor editor #" lines so +1.
    editLines <- grep("^editor editor[0-9].+$", chgs) + 1
    chgsL <- nchar(chgs[editLines]) != 0
    chgsEdit <- editLines[chgsL]
    #####################################################################
    start <- grep('contenteditable=\"true\"', src)
    endTags <- grep('</p>', src)
    end <- vector("numeric", length(start))
    
    # There could be more </p> tags than <p contenteditable="true"> tags.
    # So I'm subsetting for </p> tags that are after the <p content...>
    #  tags and finding the smallest(min) </p> tags to ensure that there
    #  are no other </p> tags between them.
    for (i in 1:length(start)) {
        end[i] <- min(endTags[start[i] < endTags])
    }
    # Since the lines to be actually editted are the ones after "start",
    #  and before "end". Hence, +1 and -1.
    srcLines <- mapply(FUN = seq, start+1, end-1)
    
    # Actual lines to be editted.
    srcEdit <- srcLines[nchar(chgs[editLines]) != 0]
    
    # Extra lines inside the <p></p> tags except the first line are removed.
    rm.lines <- vector("numeric", length(srcEdit))
    rm.lines <- unlist(lapply(1:length(srcEdit),
                              function(i) rm.lines[i] <- srcEdit[[i]][-1]))
    src <- src[-rm.lines]
    
    comment <- "<-- This paragraph has been editted -->"
    lines <- paste(chgs[chgsEdit], comment)
    
    # New locations for lines to be editted.
    temp <- grep('contenteditable=\"true\"', src) + 1
    src[temp[chgsL]] <- lines
    src
}

    
