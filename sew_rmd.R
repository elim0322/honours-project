library(knitr)
source("choose_file.R")
source("rinline.R")

# safe.Rmd -> post.Rmd
sew_rmd <- function(infile=NULL, outfile=NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
    if (!grepl("safe.Rmd$", infile)) {
        stop("infile must be a save.Rmd file")
    }
    src <- readLines(infile)
    
    #---------------------------- metadata chunks ----------------------------#
    metaChunks <- grep("^---$", src)
    if (length(metaChunks)>2) {
        stop("there are more than one metadata chunk")
    }
    metaCopy <- src[metaChunks[1]:metaChunks[2]]
    metaCopy[1] <- gsub("---", "<!--rmd_metadata", metaCopy[1])
    lastLine <- length(metaCopy)
    metaCopy[lastLine] <- gsub("---", "rmd_metadata-->", metaCopy[lastLine])
    src <- append(src, metaCopy, after=lastLine)
        
    #------------------------- inline R code chunks --------------------------#
    # Using the official pattern.
    inline <- grep(all_patterns$md$inline.code, src)
    # <<source("rinline.R")>>
    for (i in 1:length(inline)) {
        src[inline][i] <- processRinline(src[inline][i], format="md")
    }
    
    #-------------- generate a list of R code chunks to "keep" ---------------#
    R.begin <- grep(all_patterns$md$chunk.begin, src)
    R.end <- grep(all_patterns$md$chunk.end, src)
    if (length(R.begin) != length(R.end)) {
        stop ('Number of "begin.rcode" and "end.rcode" lines do not match')
    }
    
    # SIMPLIFY=TRUE (default) to store the result as a matrix.
    keepMat <- mapply(seq, R.begin, R.end, SIMPLIFY=TRUE)
    
    # Replace "<!--begin.rcode ... -->" with "<!--begin.keepcode ... -->".
    chunk.begin <- gsub(all_patterns$md$chunk.begin, "<!--begin.rmd_keepcode\\1", 
                        src[keepMat[1,]])
    # Replace "end.rcode-->" with "end.keepcode-->".
    last <- keepMat[nrow(keepMat),]
    chunk.end <- gsub(all_patterns$md$chunk.end, "end.rmd_keepcode-->", src[last])
    
    keep.list <- vector(mode="list", length=ncol(keepMat))
    for (i in 1:length(keep.list)) {
        keep.list[[i]] <- src[keepMat[,i]]
        keep.list[[i]][1] <- chunk.begin[i]
        l <- length(keep.list[[i]]) # for last lines ("end.rcode-->" lines)
        keep.list[[i]][l] <- chunk.end[i]
    }
    
    #--------------------------- write post.Rhtml ----------------------------#
    for (i in length(R.end):1) {
        src <- append(src, keep.list[[i]], after=last[i])
    }
    if (is.null(outfile)) {
        outfile <- gsub("safe.Rmd$", "post-md.Rmd", infile)
    }
    writeLines(src, outfile)
}
