library(knitr)
source("choose_file.R")

# Rmd -> pre.Rmd
# To protect Rmarkdown metadata and R chunks before passed onto pandoc.
protect <- function(infile=NULL, outfile=NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
    if (!grepl("[Rr]md$", infile)) {
        stop("infile must be an Rmd file")
    }
    src <- readLines(infile)
    
    #------------------------------- metadata --------------------------------#
    metadata <- grep("^---$", src) # metadata syntax is "---"
    # There can be only one metadata chunk per document.
    if (length(metadata)>2) {
        stop("more than one pair of metadata syntax!")
    }
    src[metadata[1]] <- gsub("^---$", "<!--rmd_metadata", src[metadata[1]])
    src[metadata[2]] <- gsub("^---$", "rmd_metadata-->", src[metadata[2]])
    
    #------------------------------- R chunks --------------------------------#
    # (There is no need to protect inline chunks as they are perfectly retainable)
    # chunk.begin lines will be: <!--begin.rmd_keepcode```{r, echo=FALSE, ...}
    # chunk.end lines will be: ```end.rmd_keepcode-->
    begin.regexpr <- paste0("(", all_patterns$md$chunk.begin, ")")
    end.regexpr <- paste0("(", all_patterns$md$chunk.end, ")")
    src <- gsub(begin.regexpr, "<!--begin.rmd_keepcode\\1", src)
    src <- gsub(end.regexpr, "\\1end.rmd_keepcode-->", src)
    
    #----------------------------- Write pre.Rmd -----------------------------#
    if (is.null(outfile)) {
        outfile <- gsub("[Rr]md$", "pre.Rmd", infile)
    }
    writeLines(src, outfile)
}
