
copyInline <- function(line) {
    temp <- gsub("^.*<!--rinline", "<!--keep.rinline", line)
    line <- gsub("-->.*$", paste("-->", temp), line)
    line
}

sew <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        infile <- load.dir()
    } 
    if (!grepl("Rhtml$", infile))
        stop("infile is not an Rhtml file")
    src <- readLines(infile)
  
    R.begin <- grep("<!--begin.rcode", src)
    R.end <- c(grep("end.rcode-->", src), length(src))
    
    ######################### inline R code chunks  #########################
    in.line <- grep("<!--rinline", src)
  
    for (i in 1:length(in.line)) {
        src[in.line][i] <- copyInline(src[in.line][i])
    }
    #########################################################################
  
    keep.list <- vector("list", (length(R.begin)+1))
  
    for(i in 1:length(R.begin)) {
        keep.list[[i]] <- src[R.begin[i]:R.end[i]]
        newFirstLine <- gsub("[.]r", ".keep", keep.list[[i]][1])
        keep.list[[i]][1] <- newFirstLine
    }

    lines <- c(1, R.end[-length(R.end)] + 1)
  
    if (is.null(outfile)) {
        outfile <- gsub("Rhtml","post.Rhtml", infile)
    }
    
    f <- file(outfile, open = "w")
    for (i in 1:length(R.end)) {
        writeLines(c(src[lines[i]:R.end[i]], keep.list[[i]]), f)
    }
    close(f)
}
