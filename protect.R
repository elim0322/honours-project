library(knitr)
# The R script "choose_file.R" contains choose.file() that
# executes file.choose() and setwd() when infile is not specified.
source("choose_file.R")

# NOTE: Source documents should be standardised using pandoc into a format that
#       is consistent through multiple conversion with pandoc so that we can
#       safely round-trip on the source document.
#       Pandoc does not know how to deal with Rmarkdown chunks and Rmarkdown
#       metadata information, and completely ignores them when the source 
#       document is standardised. Therefore they must be protected prior to
#       standardise().

# Rmd -> pre.Rmd
# To protect Rmarkdown metadata and R chunks before passing onto pandoc.
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
    
    #-------------------------------------------------------------------------#
    #--------------------------- Protect metadata ----------------------------#
    #-------------------------------------------------------------------------#
    metadata <- grep("^---$", src)     # metadata syntax is "---"
    # There can be only one metadata chunk per document.
    if (length(metadata)>2) {
        stop("more than one metadata!")
    }
    
    # NOTE: When processed by standardise(), that is standardisation by pandoc,
    #       lines are removed between "rmd_metadata-->" or "end_keepcode-->" 
    #       and text.
    #       By adding "rmd-rmLines" at the end, pandoc splits this text into a 
    #       new line. Then we can remove lines containing "rmd-rmLines" in 
    #       unprotect().
    src[metadata[1]] <- gsub("^---$", "<!--rmd_metadata", src[metadata[1]])
    src[metadata[2]] <- gsub("^---$", "rmd_metadata--> rmd-rmLines", 
                             src[metadata[2]])
    
    #-------------------------------------------------------------------------#
    #--------------------------- Protect R chunks ----------------------------#
    #-------------------------------------------------------------------------#
    # (No need to protect inline chunks as they are perfectly retainable.
    # ie, pandoc does not touch them).
    # chunk.begin lines will be: <!--begin.keepcode```{r, echo=FALSE, ...}
    # chunk.end lines will be: ```end.keepcode-->
    # so that any specified chunk options can be retained and easier processing
    # as we only need to remove that added text.
    begin.regexpr <- paste0("(", all_patterns$md$chunk.begin, ")")
    end.regexpr <- paste0("(", all_patterns$md$chunk.end, ")")
    src <- gsub(begin.regexpr, "<!--begin.keepcode\\1", src)
    src <- gsub(end.regexpr, "\\1end.keepcode--> rmd-rmLines", src)
    
    #-------------------------------------------------------------------------#
    #----------------------------- Write pre.Rmd -----------------------------#
    #-------------------------------------------------------------------------#
    if (is.null(outfile)) {
        outfile <- gsub("[Rr]md$", "pre.Rmd", infile)
    }
    writeLines(src, outfile)
}
