library(knitr)
source("choose_file.R")

# std.Rmd -> safe.Rmd
# To revert back to the original syntax.
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
    #----------------- Remove rmLines ------------------#
    # grep all "rmd-rmLines" and remove the ones that are
    # after the end lines of chunks (end.keepcode-->).
    allRmLines <- grep("^rmd-rmLines$", src)
    endLines <- grep("^.+-->$", src)
    rmLines <- allRmLines[which(allRmLines==endLines+1)]
    src <- src[-rmLines]
    
    #------------ Revert protected R chunks ------------#
    src <- gsub("<!--begin.keepcode", "", src)
    src <- gsub("end.keepcode-->", "", src)
    
    #-------------- Revert metadata chunks -------------#
    src <- gsub("<!--rmd_metadata", "---", src)
    src <- gsub("rmd_metadata-->", "---", src)
    
    #------------------- Write lines -------------------#
    if (is.null(outfile)) {
        outfile <- gsub("std.Rmd$", "safe.Rmd", infile)
    }
    writeLines(src, outfile)
}
