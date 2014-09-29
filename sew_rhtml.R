library(knitr)
# The R script "choose_file.R" contains choose.file() that
# executes file.choose() and setwd() when infile is not specified.
source("choose_file.R")
# The R script "rinline.R" contains copyInline() and processRinline()
# to copy and attach inline chunks.
source("rinline.R")

# .Rhtml -> post-RHTML.Rhtml
# To copy R chunks which are lost when processed by knit().
sew_rhtml <- function(infile=NULL, outfile=NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
    if (!grepl("[Rr]html$", infile)) {
        stop("infile must be a Rhtml file")
    }
    src <- readLines(infile)
    
    ###########################################################################
    ########################## inline R code chunks ###########################
    ###########################################################################
    # Using the official pattern.
    inline <- grep(all_patterns$html$inline.code, src)
    for (i in 1:length(inline)) {
        src[inline][i] <- processRinline(src[inline][i], format="html")
    }
    
    ###########################################################################
    ############### generate a list of R code chunks to "keep" ################
    ###########################################################################
    R.begin <- grep(all_patterns$html$chunk.begin, src)
    R.end <- grep(all_patterns$html$chunk.end, src)
    if (length(R.begin) != length(R.end)) {
        stop ('Number of "begin.rcode" and "end.rcode" lines do not match')
    }
    
    # SIMPLIFY=TRUE (default) to store the result as a matrix.
    keepMat <- mapply(seq, R.begin, R.end, SIMPLIFY=TRUE)
    
    # Replace "<!--begin.rcode ... -->" with "<!--begin.keepcode ... -->".
    chunk.begin <- gsub("(^\\s*<!--\\s*begin.)(r)(code\\s*.*)",
                        "\\1keep\\3", src[keepMat[1,]])
    # Replace "end.rcode-->" with "end.keepcode-->".
    last <- keepMat[nrow(keepMat),]
    chunk.end <- gsub("(^\\s*end.)(r)(code\\s*-->.*$)","\\1keep\\3",src[last])
    
    # Generate a list of copied lines to keep.
    keep.list <- vector(mode="list", length=ncol(keepMat))
    for (i in 1:length(keep.list)) {
        keep.list[[i]] <- src[keepMat[,i]]
        keep.list[[i]][1] <- chunk.begin[i]
        l <- length(keep.list[[i]]) # for last lines ("end.rcode-->" lines)
        keep.list[[i]][l] <- chunk.end[i]
    }
    
    ###########################################################################
    ######################### write post-RHTML.Rhtml ##########################
    ###########################################################################
    # Outfile has the suffix, ".post-RHTML.Rmd", to keep track of the initial
    # source doc format as both knit() and rmarkdown::render() will produce 
    # .post.html for confusion.
    for (i in length(R.end):1) {
        src <- append(src, keep.list[[i]], after=last[i])
    }
    if (is.null(outfile)) {
        outfile <- gsub("[Rr]html", "post-RHTML.Rhtml", infile)
    }
    writeLines(src, outfile)
}
