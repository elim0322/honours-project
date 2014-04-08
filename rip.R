rip <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        infile <- load.dir()
    }
    if (!grepl("save.html$", infile))
        stop("infile is not a save.html file")
    
    src <- readLines(infile)
    
    ########################## Remove style #########################
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
    chunks.begin <- grep('^<div class=\"chunk\" id=\".*$', src)
    # We assume that ONLY knitr has written </div></div> on a line
    chunks.end <- grep('^</div></div>$', src)
    # Extra protection
    chunkLine <- grep('^</pre></div>$', src)
    # Find which chunks.end lines are the right ones.
    # ie, chunks.end lines that are after chunkLine lines.
    #chunks.end <- chunkLine[match(chunks.end, (chunkLine+1))]
    chunks.end <- chunkLine[(chunkLine+1) %in% chunks.end]+1
    
    lines <- mapply(seq, chunks.begin, chunks.end)
    src <- src[-unlist(lines)]
    
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
    
    ## Remove jQuery, CKEditor, annotator.js and their functions ###
    # ASSUME: they are always inserted at these lines
    src <- src[-c(4:88)]
    
    ####################### Remove the buttons ######################
    temp <- grep("<p id=\"savebutton\" style=\"background-", src)
    # ASSUME: 6 lines of buttons.
    src <- src[-c(temp:(temp+5))]
    
    ####################### Write return.Rhtml ######################
    if (is.null(outfile)) {
        outfile <- gsub("save.html","return.Rhtml", infile)
    }
    write(src, outfile)
}
