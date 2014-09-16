# The R script "choose_file.R" contains choose.file() that
# executes file.choose() and setwd() when infile is not specified.
source("choose_file.R")

# ript.Rmd -> return.Rmd (to tidy up things after inverting with pandoc
# which are mainly tabs inserted for chunks)
tidy <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
    if (!grepl("ript.Rmd$", infile)) {
        stop("infile must be a ript.Rmd file")
    }
    src <- readLines(infile)
    
    #------------------------------ Remove tabs ------------------------------#
    # Find the lines with tabs and remove the tabs.
    tabLines <- grep("^\\s\\s\\s\\s", src)
    src[tabLines] <- gsub("^\\s\\s\\s\\s", "", src[tabLines])
    
    #-------------------------- Remove inline tabs ---------------------------#
    # Find inline chunk lines (this is a reliable search as it's searching 
    # within the subset of src that contains tabs.
    inline <- grep("`r.+$", src[tabLines])
    inline <- tabLines[inline]
    
    # Inline chunks are separated with empty spaces like proper chunks like:
    # text1
    # 
    #     `r summary(cars)`
    #
    # text2
    
    # Generate inline chunk lines as a list and rip out old lines and append
    # correctly formatted lines (new).
    inline.list <- mapply(seq, inline-2, inline+2, SIMPLIFY=FALSE)
    for (i in length(inline):1) {
        oldLines <- inline.list[[i]]
        # newLines are + and -2 lines to get rid of empty spaces.
        newLines <- paste(src[inline[i]-2], src[inline[i]], src[inline[i]+2])
        firstOldLine <- oldLines[1]
        src <- append(src[-oldLines], newLines, after=firstOldLine - 1)
    }
        
    #------------------------------ Write file -------------------------------#
    if (is.null(outfile)) {
        outfile <- gsub("ript.Rmd","return.Rmd", infile)
    }
    f <- file(outfile)
    # cat() is used here to remove (or hide from display) the backslash escapes
    # inserted by R for double quotes within the string.
    # [1] "This is an \"example\" (we don't want this as the source doc doesn't
    # have the escapes)
    # NOTE: cat() except the line line to avoid adding an extra empty space.
    cat(src[-length(src)], sep="\n", file=f)
    close(f)
}
