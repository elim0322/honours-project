library(knitr)
source("choose_file.R")

# pre.Rmd -> std.Rmd
# Standardising the format of input document using pandoc.
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
    
    # Create appropriate command line
    cmd <- paste("pandoc -f markdown -t markdown --no-wrap -o", outfile, infile)
    system(cmd)
}
