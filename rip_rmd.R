# The R script "choose_file.R" contains choose.file() that
# executes file.choose() and setwd() when infile is not specified.
source("choose_file.R")

# For the sake of minimising confusion, the steps below are all done in 
# rip_rmd(), instead of separating them out into different functions. These
# tasks are essentially doing what rip() is supposed to do, just in different
# ways we are dealing with different format.
#
# 1. save-RMD.html -> post.save-RMD.html
#    (This is the usual rip process to remove contenteditable attributes and
#    things)
# 2. post.save-RMD.html --(pandoc)--> post.save-RMD.Rmd
#    (Using pandoc to remove <script>s is so much easier and more reliable than
#    text-processing them so it may be worthwhile to involve pandoc)
# 3. post.save-RMD.Rmd --> return.Rmd
#    (pandoc introduces things such as adding tabs for chunks so these are
#    removed)

# save-RMD.html -> post.save-RMD.html -> post.save-RMD.Rmd
# To rip out contents added by rmarkdown.
rip_rmd <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
    if (!grepl("save-RMD.html$", infile)) {
        stop("infile must be a save-RMD.html file")
    }
    src <- readLines(infile)
    
    #-------------------------------------------------------------------------#
    #------------------------- Remove R chunk output -------------------------#
    #-------------------------------------------------------------------------#
    # Assuming that ONLY render() has written <pre><code>... for R Code Chunks
    # NOTE: R code is delimited by <pre class="r"><code> ... </code></pre>.
    #       Output is delimited by <pre><code> ... </code></pre>
    chunks.begin <- grep('^<pre.*><code>.+$', src)
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
    
    #-------------------------------------------------------------------------#
    #----------------------------- Fix <p><img> ------------------------------#
    #-------------------------------------------------------------------------#
    # Plots generated from R is stored in <p><img>. The whole chunk copy gets 
    # included in the <p><img></></p>.
    # eg, "<p><img> ... </> <!--begin.keepcode
    #      ...R code...
    #      end.keepcode--></p>"
    
    # Find the beginning and closing lines and generate a list of sequence,
    # if there are any.
    if (any(grepl("^<p><img.+$", src))) {
        img.begin <- grep("^<p><img.+$", src)
        img.end <- grep("end[.]keepcode-->.+$", src)
        img.chunks <- mapply(seq, img.begin, img.end, SIMPLIFY=FALSE)
        for (i in length(img.chunks):1) {
            oldLines <- img.chunks[[i]]
            newLines <- src[unlist(img.chunks[[i]])]
            # Remove all stuff before "<!--begin.keepcode" from the first line.
            newLines[1] <- gsub("^.*(<!--begin[.]keepcode.+$)", "\\1", newLines[1])
            # Remove the closing paragraph tags at the end of "end.keepcode-->"
            last <- length(newLines)
            newLines[last] <- gsub("(^.*end[.]keepcode-->).+$", 
                                   "\\1", newLines[last])
            
            firstOldLine <- oldLines[1]
            src <- append(src[-oldLines], newLines, after=firstOldLine-1)
        }
    }
        
    # NOTE: There is no need to remove the scripts of jQuery, CKEditor and
    #       annotator.js as they are removed by pandoc (lines 151-154).
    
    #-------------------------------------------------------------------------#
    #-------------------------- Remove the buttons ---------------------------#
    #-------------------------------------------------------------------------#
    # Similarly, search for the starting line of "button.html", then
    #  remove from that line up to the end line
    button.start <- grep("<p id=\"savebutton\" style=\"background-", src)
    button.end <- button.start +  length(readLines("button.html")) - 1
    src <- src[-c(button.start:button.end)]
    
    #-------------------------------------------------------------------------#
    #-------------------- Remove 'contenteditable="true"' --------------------#
    #-------------------------------------------------------------------------#
    srcEditLines <- grep('contenteditable="true"', src)
    # We know that one empty space is in front of 'contenteditable="true"'
    # from the way snap() adds it.
    src[srcEditLines] <- gsub('(^.*?<.*)\\scontenteditable="true"',
                              "\\1", src[srcEditLines])
    
    #-------------------------------------------------------------------------#
    #------------------------- Remove 'id="Editor-' --------------------------#
    #-------------------------------------------------------------------------#
    srcEditLines <- grep('id=\"Editor-', src)
    src[srcEditLines] <- gsub(' id="Editor-[0-9]+"', "", src[srcEditLines])
    
    # NOTE: all nodes inside <head> are removed by pandoc so no need to remove
    #       them.
    
    #-------------------------------------------------------------------------#
    #--------------------------- Remove all <div>s ---------------------------#
    #-------------------------------------------------------------------------#
    # pandoc doesn't remove div nodes.
    divs <- grep("^.*(<|</)div.*>.*$", src)
    src <- src[-divs]
    
    #-------------------------------------------------------------------------#
    #---------------------- Change <!--rmd_metadata--> -----------------------#
    #-------------------------------------------------------------------------#
    # Wrapping things inside <pre> seems to work like the verbatim environment
    # for pandoc. The output from pandoc will have a tab for each line in <pre>
    src <- gsub("^<!--rmd_metadata$", "<pre>---", src)
    src <- gsub("^rmd_metadata-->$", "---</pre>", src)
    
    #-------------------------------------------------------------------------#
    #---------------------------- Change R chunks ----------------------------#
    #-------------------------------------------------------------------------#
    src <- gsub("^<!--begin[.]keepcode", "<pre>", src)
    src <- gsub("end[.]keepcode-->$", "</pre>", src)
    
    #-------------------------------------------------------------------------#
    #------------------------- Change inline chunks --------------------------#
    #-------------------------------------------------------------------------#
    # Assume that "<em>(inline_output:...)</em>" is only inserted by snap_rmd()
    src <- gsub("(^.*)<em>\\(inline_output:.+\\)</em>(.*$)", "\\1\\2", src)
    inline <- grep("^.*<!--rinline[.]keep.+$", src)
    src[inline] <- gsub("<!--rinline[.]keep", "<pre>`r ", src[inline])
    src[inline] <- gsub("-->", "`</pre>", src[inline])
    
    #-------------------------------------------------------------------------#
    #----------------------- Write post.save-RMD.html ------------------------#
    #-------------------------------------------------------------------------#
    temp <- gsub("save-RMD.html","post.save-RMD.html", infile)
    writeLines(src, temp)
    
    #-------------------------------------------------------------------------#
    #----------------- Write post.save-RMD.Rmd using pandoc ------------------#
    #-------------------------------------------------------------------------#
    pandocOutfile <- gsub("html$", "Rmd", temp)
    cmd <- paste("pandoc -f html -t markdown --no-wrap -o",
                 pandocOutfile, temp)
    system(cmd)
    
    #-------------------------------------------------------------------------#
    #------------------------------ Remove tabs ------------------------------#
    #-------------------------------------------------------------------------#
    src <- readLines(pandocOutfile)
    # Find the lines with tabs and remove the tabs.
    tabLines <- grep("^\\s\\s\\s\\s", src)
    src[tabLines] <- gsub("^\\s\\s\\s\\s", "", src[tabLines])
    
    #-------------------------------------------------------------------------#
    #-------------------------- Remove inline tabs ---------------------------#
    #-------------------------------------------------------------------------#
    # Find inline chunk lines (this is a reliable search as it's searching 
    # within the subset of src that contains tabs.
    inline <- grep("`r.+$", src[tabLines])
    
    # Inline chunks are separated with empty spaces like proper chunks like:
    # text1
    # 
    #     `r summary(cars)`
    #
    # text2
    
    # Generate inline chunk lines as a list and rip out old lines and append
    # correctly formatted lines (new).
    if (length(inline)>0) {
        inline <- tabLines[inline]
        inline.list <- mapply(seq, inline-2, inline+2, SIMPLIFY=FALSE)
        for (i in length(inline):1) {
            oldLines <- inline.list[[i]]
            # newLines are + and -2 lines to get rid of empty spaces.
            newLines <- paste(src[inline[i]-2], src[inline[i]], src[inline[i]+2])
            firstOldLine <- oldLines[1]
            src <- append(src[-oldLines], newLines, after=firstOldLine - 1)
        }
    }
        
    #-------------------------------------------------------------------------#
    #------------------------------ Write file -------------------------------#
    #-------------------------------------------------------------------------#
    if (is.null(outfile)) {
        outfile <- gsub("post.save-RMD.Rmd","return.Rmd", pandocOutfile)
    }
    f <- file(outfile)
    # cat() is used here to remove (or hide from display) the backslash escapes
    # inserted by R for double quotes within the string.
    # [1] "This is an \"example\" (we don't want this as the source doc doesn't
    # have the escapes)
    cat(src, sep="\n", file=f)
    close(f)
}
