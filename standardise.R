library(knitr)
# The R script "choose_file.R" contains choose.file() that
# executes file.choose() and setwd() when infile is not specified.
source("choose_file.R")

# pre.Rmd -> std.Rmd
# To standardise the input document using pandoc.
standardise <- function(infile=NULL, outfile=NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
    if (!grepl("pre.Rmd$", infile)) {
        stop("infile must be a pre.Rmd file")
    }
    src <- readLines(infile)
    
    if (is.null(outfile)) {
        outfile <- gsub("pre.Rmd$", "std.Rmd", infile)
    }
    
    #---------------------------- pandoc command -----------------------------#
    # -f: from markdown format
    # -t: to markdown format
    # --no-wrap: no text wrapping
    # Create a string of command line for pandoc and run it with system().
    cmd <- paste("pandoc -f markdown -t markdown --no-wrap -o", 
                 outfile, infile)
    system(cmd)
}
