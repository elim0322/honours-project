# The R script "choose_file.R" contains choose.file() that
# executes file.choose() and setwd() when infile is not specified.
source("choose_file.R")

# save-RHTML.html -> return.Rhtml
# To rip out the contents added by knitr.
rip_rhtml <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
    if (!grepl("save-RHTML.html$", infile)) {
        stop("infile must be a save-RHTML.html file")
    }
    src <- readLines(infile)
    
    ###########################################################################
    ############################## Remove style ###############################
    ###########################################################################
    # We assume that ONLY knitr has written .knitr.inline within a <style>
    # ".knitr.inline {" string seems to be always after 
    # "<style type=\"text/css\">" string.
    # Using this property, the correct "<style..." string is removed.
    style.inline <- grep("[.]knitr[.]inline [{]", src)
    style.begin <- grep('<style type=\"text/css\">', src)
    style.end <- grep("</style>", src)
    if (any(style.begin == (style.inline-1))) {
        # We use [1] to guard against multiple matching style.begins
        start <- style.begin[which(style.begin == (style.inline-1))[1]]
        end <- style.end[which(style.end > start)[1]]
        src <- src[-c(start:end)]
    }
    
    ###########################################################################
    ##################### Remove "<div class=..." strings #####################
    ###########################################################################
    # We assume that ONLY knitr has written <div class="chunk"
    chunks.begin <- grep('^<div class=\"chunk\" id=\".*$', src)
    # We assume that ONLY knitr has written </div></div> on a line
    chunks.end <- grep("^.*</div></div>$", src)
    # Extra protection
    chunkLine <- grep("^.*</pre></div>$", src)
    # Find which chunks.end lines are the right ones.
    # ie, chunks.end lines that are after chunkLine lines.
    chunks.end <- chunkLine[(chunkLine+1) %in% chunks.end]+1
    
    lines <- mapply(seq, chunks.begin, chunks.end)
    src <- src[-unlist(lines)]
    
    ###########################################################################
    ########################### Remove inline codes ###########################
    ###########################################################################
    inline <- grep('<code class=\"knitr inline\">', src)
    src[inline] <- gsub('<code class=\"knitr inline"(.+?)</code>', 
                        "", src[inline])
    
    ###########################################################################
    ########################### Replace all "keep"s ###########################
    ###########################################################################
    src <- gsub("(.+[.])(keep)(code.*$)", "\\1r\\3", src)
    src <- gsub("<!--keep.rinline", "<!--rinline", src)
    
    ###########################################################################
    ####################### Remove extra spaces added #########################
    ###########################################################################
    # An empty space seems to be added before each R chunk
    extra <- grep("<!--begin.rcode", src)-1
    src <- src[-extra]
    
    ## Remove jQuery, CKEditor, annotator.js and their functions ###
    # Search for the starting line of "edit.js" in src, then remove
    #  from that line up to the end line of edit.js (which should be
    #  the starting line + length(edit.js) - 1 to take duplicated
    #  first lines into account).
    editJs.start <- grep("<!-- loading jQuery, CKEditor, and annotator.js -->",
                         src)
    editJs.end <- editJs.start + length(readLines("edit.js")) - 1
    src <- src[-c(editJs.start:editJs.end)]
    
    ###########################################################################
    ########################### Remove the buttons ############################
    ###########################################################################
    # Similarly, search for the starting line of "button.html", then
    #  remove from that line up to the end line
    button.start <- grep("<p id=\"savebutton\" style=\"background-", src)
    button.end <- button.start +  length(readLines("button.html")) - 1
    src <- src[-c(button.start:button.end)]
    
    ###########################################################################
    ##################### Remove 'contenteditable="true"' #####################
    ###########################################################################
    srcEditLines <- grep('contenteditable="true"', src)
    # We know that one empty space is in front of 'contenteditable="true"'
    #  from the way snap() adds it.
    src[srcEditLines] <- gsub('(^.*?<.*)\\scontenteditable="true"',
                              "\\1", src[srcEditLines])
    
    ###########################################################################
    ########################## Remove 'id="Editor-' ###########################
    ###########################################################################
    srcEditLines <- grep('id=\"Editor-', src)
    src[srcEditLines] <- gsub(' id="Editor-[0-9]+"', "", src[srcEditLines])
    
    ###########################################################################
    ########################### Write return.Rhtml ############################
    ###########################################################################
    if (is.null(outfile)) {
        outfile <- gsub("save-RHTML.html","return.Rhtml", infile)
    }
    writeLines(src, outfile)
}
