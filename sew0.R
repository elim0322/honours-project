# this is sew0() to protect the metadata information..
# Rmd -> pre.Rmd

sew0 <- function(infile=NULL, outfile=NULL) {
    if (is.null(infile)) {
        infile <- load.dir()
    } 
    if (!grepl("Rmd$", infile))
        stop("infile is not an Rmd file")
    
    #----- metadata
    src <- readLines(infile)
    metadata <- grep("^---$", src) # metadata syntax is "---"
    # There can be only one metadata chunk per document.
    if (length(metadata)!=2) {
        stop("more than one pair of metadata syntax!")
    }
    src[metadata[1]] <- gsub("---", "<!-- metadata", src[metadata[1]])
    src[metadata[2]] <- gsub("---", "metadata -->", src[metadata[2]])
    
    #----- r code chunks
    chunksStart <- grep("^\\s*```+\\s*\\{r", src)
    chunksEnd <- grep("^\\s*```+\\s*$", src)
    chunksEnd2 <- vector("numeric", length=length(chunksStart))
    for (i in 1:length(chunksStart)) {
        n <- min(which(chunksStart[i]<chunksEnd))
        chunksEnd2[i] <- chunksEnd[n]
    }
    chunks <- mapply(FUN=seq, chunksStart, chunksEnd2, SIMPLIFY=FALSE)
    for (i in chunks) {
        src[i][1] <- gsub("```+", "<!--begin.rcode", src[i][1])
        last <- length(i)
        src[i][last] <- gsub("```+", "end.rcode-->", src[i][last])
    }
    
    #----- inline chunks
    src <- gsub("(`)r( +[^`]+\\s*)(`)", "<!--rinline\\2-->", src)
    
    if (is.null(outfile)) {
        outfile <- gsub("Rmd$", "pre.Rmd", infile)
    }
    writeLines(src, outfile)
}

# pre.Rmd -> std.Rmd
system("pandoc -f markdown -t markdown -o example3.std.Rmd example3.pre.Rmd")

