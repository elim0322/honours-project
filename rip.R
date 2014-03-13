rip <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        infile <- load.dir()
    }
    if (!grepl("post.html$", infile))
        stop("infile is not a post.html file")
    
    src <- readLines(infile)

    # We assume that ONLY knitr has written .knitr.inline within a <style>
    # ".knitr.inline {" string seems to be always after 
    # "<style type=\"text/css\">" string.
    # Using this property, the correct "<style..." string is removed.
    inline <- grep("[.]knitr[.]inline [{]", src)
    style.begin <- grep('<style type=\"text/css\">', src)
    style.end <- grep("</style>", src)
    
    if (any(style.begin == (inline-1))) {
        # We use [1] to guard against multiple matching style.begins
        start <- style.begin[which(style.begin == (inline-1))[1]]
        end <- style.end[which(style.end > start)[1]]
        src <- src[-c(start:end)]
    }
    
    ################ Remove "<div class=..." strings ################
    # We assume that ONLY knitr has written <div class="chunk"
    chunks.begin <- grep('<div class=\"chunk\".+$', src)
    # We assume that ONLY knitr has written </div></div> on a line
    chunks.end <- grep('^</div></div>$', src)
    # Extra protection
    chunkLine <- grep('^</pre></div>$', src)
    # Find which chunks.end lines are the right ones.
    # ie, chunks.end lines that are after chunkLine lines.
    chunks.end <- chunkLine[match(chunks.end, (chunkLine+1))]
    
    if (length(chunks.begin) == length(chunks.end)) {
        # Can use this line instead: src <- src[-mapply(seq, chunks.begin, chunks.end)]
        chunks <- vector("list", length(chunks.begin))
        for (i in 1:length(chunks.begin)) {
            chunks[[i]] <- chunks.begin[i]:chunks.end[i]
        }
    }
    lines <- unlist(chunks)
    src <- src[-lines]
    
    ###################### Remove inline codes ######################
    inline <- grep('<code class=\"knitr inline\">', src)
    src[inline] <- gsub('<code class=\"knitr inline"(.+?)</code>', 
                        "", src[inline])
    
    ###################### Replace all "keep"s ######################
    src <- gsub("<!--begin.keepcode", "<!--begin.rcode", src)
    src <- gsub("<!--keep.rinline", "<!--rinline", src)
    
    ################### Remove extra spaces added ###################
    # An empty space seems to be added before each R chunk
    extra <- grep("<!--begin.rcode", src)-1
    src <- src[-extra]
    
    ####################### Write return.Rhtml ######################
    if (is.null(outfile)) {
        outfile <- gsub("post.html","return.Rhtml", infile)
    }
    write(src, outfile)
}
