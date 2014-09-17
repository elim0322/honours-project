# The R script "choose_file.R" contains choose.file() that
# executes file.choose() and setwd() when infile is not specified.
source("choose_file.R")

# "rip" process takes an additional step:
# 1. save-md.html -> ript.html
#    (This is the usual rip process to remove contenteditable attributes and
#    things)
# 2. ript.html --(pandoc)--> ript.Rmd
#    (Using pandoc to remove <script>s is so much easier and more reliable than
#    text-processing them so it may be worthwhile to involve pandoc)

# Then tidy() will tidy up .ript.Rmd to 
# 3. ript.Rmd --> return.Rmd
#    (pandoc introduces things such as adding tabs for chunks so these are
#    removed)

# save-RMD.html -> return.Rmd
rip_rmd <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
#     if (!grepl("save-RMD.html$", infile)) {
#         stop("infile must be a save-RMD.html file")
#     }
    src <- readLines(infile)
    
    #-------------------- Remove '<pre class="r"' strings --------------------#
    # Assuming that ONLY render() has written <pre class="r"
    chunks.begin <- grep('^<pre class="r"', src)
    # Assuming that ONLY render() has written </code></pre>
    ends <- grep("^.*</code></pre>$", src)
    # Find which chunks.end lines are the right ones.
    # ie, chunks.end lines that are after chunkLine lines.
    chunks.end <- vector("numeric", length=length(chunks.begin))
    chunks.end[length(chunks.end)] <- ends[length(ends)]
    if (length(chunks.begin)>1) {
        for (i in 1:(length(chunks.begin)-1)) {
            whichEnd <- which((chunks.end > chunks.begin[i]) & 
                                  (chunks.end < chunks.begin[i+1]))
            chunks.end[i] <- ends[whichEnd]
        }
    }
    lines <- mapply(seq, chunks.begin, chunks.end)
    src <- src[-unlist(lines)]
    
    #----------------------------- Fix <p><img> ------------------------------#
    # Plots generated from R is stored in <p><img>.
    # First line of the chunk copy (<!--begin.keepcode) gets also included in
    # the <p>. So remove anything before "<!--begin.keepcode..." (ie, remove
    # <p><img>...).
    # Make it a bit more robust by searching specifically for "<p><img..."
    img.begin <- grep("^<p><img.+$", src)
    src[img.begin] <- gsub("^.*(<!--begin[.]keepcode.+$)", "\\1",
                           src[img.begin])
    
    # "end.keepcode-->" is followed by text:
    #   eg, "<p><img...> ... </> end.keepcode--> ...next line... </p>"
    img.end <- grep("end[.]keepcode-->.+$", src)
    img.endList <- strsplit(src[img.end], "end[.]keepcode-->")
    for (i in length(img.endList):1) {
        # Split by "end.keepcode-->" so need to paste it back.
        img.endList[[i]][1] <- paste0(img.endList[[i]][1],
                                      "end.keepcode-->")
        # "<p>" is removed by gsub so need to add it back
        # to match the end </p> tag.
        img.endList[[i]][2] <- paste0("<p>", img.endList[[i]][2])
        oldLines <- img.end[i]
        newLines <- img.endList[[i]]
        firstOldLine <- oldLines[1]
        src <- append(src[-oldLines], newLines, after=firstOldLine-1)
    }
    
    #------- Remove jQuery, CKEditor, annotator.js and their functions -------#
    # Search for the starting line of "edit.js" in src, then remove
    #  from that line up to the end line of edit.js (which should be
    #  the starting line + length(edit.js) - 1 to take duplicated
    #  first lines into account).
    editJs.start <- grep("<!-- loading jQuery, CKEditor, and annotator.js -->",
                         src)
    editJs.end <- editJs.start + length(readLines("edit.js")) - 1
    src <- src[-c(editJs.start:editJs.end)]
    
    #-------------------------- Remove the buttons ---------------------------#
    # Similarly, search for the starting line of "button.html", then
    #  remove from that line up to the end line
    button.start <- grep("<p id=\"savebutton\" style=\"background-", src)
    button.end <- button.start +  length(readLines("button.html")) - 1
    src <- src[-c(button.start:button.end)]
    
    #-------------------- Remove 'contenteditable="true"' --------------------#
    srcEditLines <- grep('contenteditable="true"', src)
    # We know that one empty space is in front of 'contenteditable="true"'
    #  from the way snap() adds it.
    src[srcEditLines] <- gsub('(^.*?<.*)\\scontenteditable="true"',
                              "\\1", src[srcEditLines])
    #------------------------- Remove 'id="Editor-' --------------------------#
    srcEditLines <- grep('id=\"Editor-', src)
    src[srcEditLines] <- gsub(' id="Editor-[0-9]+"', "", src[srcEditLines])
    
    # NOTE: all nodes inside <head> are removed by pandoc so no need to remove
    #       them.
    
    #--------------------------- Remove all <div>s ---------------------------#
    # pandoc doesn't remove div nodes.
    divs <- grep("^.*(<|</)div.*>.*$", src)
    src <- src[-divs]
    
    #---------------------- Change <!--rmd_metadata--> -----------------------#
    # Wrapping things inside <pre> seems to work like the verbatim environment
    # for pandoc. The output from pandoc will have a tab for each line in <pre>
    src <- gsub("^<!--rmd_metadata$", "<pre>---", src)
    src <- gsub("^rmd_metadata-->$", "---</pre>", src)
    
    #---------------------------- Change R chunks ----------------------------#
    src <- gsub("^<!--begin[.]keepcode", "<pre>", src)
    src <- gsub("end[.]keepcode-->$", "</pre>", src)
    
    #------------------------- Change inline chunks --------------------------#
    # Assume that "<em>(inline_output:...)</em>" is only inserted by snap_rmd()
    src <- gsub("(^.*)<em>\\(inline_output:.+\\)</em>(.*$)", "\\1\\2", src)
    inline <- grep("^.*<!--rinline[.]keep.+$", src)
    src[inline] <- gsub("<!--rinline[.]keep", "<pre>`r ", src[inline])
    src[inline] <- gsub("-->", "`</pre>", src[inline])
    
    #--------------------------- Write return.html ---------------------------#
    if (is.null(outfile)) {
        outfile <- gsub("edit-RMD.html","ript.html", infile)
    }
    write(src, outfile)
    
    #--------------------- Write return.Rmd using pandoc ---------------------#
    pandocOutfile <- gsub("html$", "Rmd", outfile)
    cmd <- paste("pandoc -f html -t markdown --no-wrap -o",  pandocOutfile, 
                 outfile)
    system(cmd)
}
