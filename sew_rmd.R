library(knitr)
# The R script "choose_file.R" contains choose.file() that
# executes file.choose() and setwd() when infile is not specified.
source("choose_file.R")
# The R script "rinline.R" contains copyInline() and processRinline()
# to copy and attach inline chunks.
source("rinline.R")

# safe.Rmd -> post-RMD.Rmd
sew_rmd <- function(infile=NULL, outfile=NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
    if (!grepl("safe.Rmd$", infile)) {
        stop("infile must be a safe.Rmd file")
    }
    src <- readLines(infile)
    
    #---------------------------- metadata chunks ----------------------------#
    metaChunks <- grep("^---$", src)
    if (length(metaChunks)>2) {
        stop("there are more than one metadata chunk")
    }
    # IF there is no metadata chunk, skip
    if (length(metaChunks) > 0) {
        metaCopy <- src[metaChunks[1]:metaChunks[2]]
        metaCopy[1] <- gsub("---", "<!--rmd_metadata", metaCopy[1])
        last <- length(metaCopy)
        metaCopy[last] <- gsub("---", "rmd_metadata-->", metaCopy[last])
        src <- append(src, metaCopy, after=last)
    }
    
    #------------------------- inline R code chunks --------------------------#
    # Using the official pattern.
    inline <- grep(all_patterns$md$inline.code, src)
    # <<source("rinline.R")>>
    for (i in 1:length(inline)) {
        src[inline][i] <- processRinline(src[inline][i], format="md")
        # When processed by rmarkdown::render(), inline output is converted as
        # normal text so cannot keep track of which bit of text is the result
        # from the inline chunk.
        # eg, "Example: `r 1+1`" becomes "Example: 2"
        # So change it to be "Example: *(inline_output: `r 1+1`)*"
        #   NOTE: * is the syntax for emphasis in markdown.
        
        # This is done in two steps:
        #  1. Put "*(inline_output: " in front of `r ...`.
        #  2. Put ")*" at the end of 'r ...' which is always followed by
        #     the copy "<!--rinline.keep...".
        src[inline][i] <- gsub("(^.*)`r\\s(.+$)", "\\1*(inline_output: `r \\2",
                               src[inline][i])
        src[inline][i] <- gsub("(^.+)`(<!--.+$)", "\\1`)*\\2", src[inline][i])
    }
    
    #-------------- generate a list of R code chunks to "keep" ---------------#
    R.begin <- grep(all_patterns$md$chunk.begin, src)
    R.end <- grep(all_patterns$md$chunk.end, src)
    if (length(R.begin) != length(R.end)) {
        stop ('Number of "begin.rcode" and "end.rcode" lines do not match')
    }
    
    # SIMPLIFY=TRUE (default) to store the result as a matrix.
    keepMat <- mapply(seq, R.begin, R.end, SIMPLIFY=TRUE)
    
    # Insert "<!--begin.keepcode" in front of "```{r}" and
    # "end.keepcode-->" at the end of ```.
    # ie, wrap whole chunks with the delimiters.
    chunk.begin <- gsub(paste0("(",all_patterns$md$chunk.begin,")"),
                        "<!--begin.keepcode\\1", src[keepMat[1,]])
    # Replace "end.rcode-->" with "end.keepcode-->".
    last <- keepMat[nrow(keepMat),]
    chunk.end <- gsub(paste0("(",all_patterns$md$chunk.end,")"),
                      "\\1end.keepcode-->", src[last])
    # Generate a list to keep.
    keep.list <- vector(mode="list", length=ncol(keepMat))
    for (i in 1:length(keep.list)) {
        keep.list[[i]] <- src[keepMat[,i]]
        keep.list[[i]][1] <- chunk.begin[i]
        l <- length(keep.list[[i]]) # for last lines ("end.rcode-->" lines)
        keep.list[[i]][l] <- chunk.end[i]
    }
    
    #--------------------------- write post-MD.Rmd ---------------------------#
    # outfile is .post-RMD.Rmd to keep track of the initial source doc format.
    # Both knit() and rmarkdown::render() will produce post.html for confusion.
    for (i in length(R.end):1) {
        src <- append(src, keep.list[[i]], after=last[i])
    }
    if (is.null(outfile)) {
        outfile <- gsub("safe.Rmd", "post-RMD.Rmd", infile)
    }
    writeLines(src, outfile)
}
