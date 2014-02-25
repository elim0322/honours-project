copyInline <- function(line) {
    # NOTE that the regular expression below is the "official"
    # pattern for knitr inline R code (all_patterns$html$inline.code)
    temp <- gsub("<!--\\s*rinline", "<!--keep.rinline", line)
    paste(line, temp)
}

processRinline <- function(line) {
    mark <- gsub("(<!--rinline(.+?)-->)", "~MARKER~\\1~MARKER~", line)
    temp <- unlist(strsplit(mark, "~MARKER~"))
    lines <- grep("<!--rinline", temp)
    for (i in 1:length(lines)) {
      temp[lines][i] <- copyInline(temp[lines][i])
    }
    result <- paste(temp, collapse = "")
    result
}


sew <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        infile <- load.dir()
    } 
    if (!grepl("Rhtml$", infile))
        stop("infile is not an Rhtml file")
    src <- readLines(infile)
    
    ######################### inline R code chunks  #########################
    in.line <- grep("<!--rinline", src)
    
    for (i in 1:length(in.line)) {
        src[in.line][i] <- processRinline(src[in.line][i])
    }
    
    ################# generate a list to keep R code chunks #################
    R.begin <- grep("<!--begin.rcode", src)
    R.end <- c(grep("end.rcode-->", src), length(src))
    
    keep.list <- vector("list", (length(R.begin)+1))
    
    for(i in 1:length(R.begin)) {
        keep.list[[i]] <- src[R.begin[i]:R.end[i]]
        newFirstLine <- gsub("[.]r", ".keep", keep.list[[i]][1])
        keep.list[[i]][1] <- newFirstLine
    }
    
    ############################ write post.Rhtml ###########################
    if (is.null(outfile)) {
        outfile <- gsub("Rhtml","post.Rhtml", infile)
    }
    
    lines <- c(1, R.end[-length(R.end)] + 1)
    
    f <- file(outfile, open = "w")
    for (i in 1:length(R.end)) {
        writeLines(c(src[lines[i]:R.end[i]], keep.list[[i]]), f)
    }
    close(f)
}
