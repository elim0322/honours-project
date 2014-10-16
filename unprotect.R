library(knitr)
# The R script "choose_file.R" contains choose.file() that
# executes file.choose() and setwd() when infile is not specified.
source("choose_file.R")

# std.Rmd -> safe.Rmd
# To revert the protected sections back to the original Rmarkdown syntax.
unprotect <- function(infile=NULL, outfile=NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
    if (!grepl("std.Rmd$", infile)) {
        stop("infile must be a std.Rmd file")
    }
    src <- readLines(infile)
    
    #-------------------------------------------------------#
    #------------------- Remove rmLines --------------------#
    #-------------------------------------------------------#
    # grep all "rmd-rmLines" and remove the ones that are
    # after the end lines of chunks (end.keepcode-->).
    allRmLines <- grep("^rmd-rmLines$", src)
    endLines <- grep("^.+-->$", src)
    rmLines <- allRmLines[which(allRmLines==endLines+1)]
    src <- src[-rmLines]
    
    #-------------------------------------------------------#
    #-------------- Revert protected R chunks --------------#
    #-------------------------------------------------------#
    src <- gsub("<!--begin.keepcode(```\\{r.+$)", "\\1", src)
    src <- gsub("end.keepcode-->", "", src)
    
    #-------------------------------------------------------#
    #---------------- Revert metadata chunks ---------------#
    #-------------------------------------------------------#
    src <- gsub("<!--rmd_metadata", "---", src)
    src <- gsub("rmd_metadata-->", "---", src)
    
    #-------------------------------------------------------#
    #------------------ Remove empty line ------------------#
    #-------------------------------------------------------#
    # Pandoc addes an extra empty line at the end of file.
    # Look for it and remove it.
    if (nchar(src[length(src)]) == 0) {
        src <- src[-length(src)]
    }
    
    #-------------------------------------------------------#
    #------------------- Write .safe.Rmd -------------------#
    #-------------------------------------------------------#
    if (is.null(outfile)) {
        outfile <- gsub("std.Rmd$", "safe.Rmd", infile)
    }
    writeLines(src, outfile)
}
