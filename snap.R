snap <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        infile <- load.dir()
    }
    if (!grepl("post.html$", infile))
        stop("infile is not a post.html file")
    
    src <- readLines(infile)
    
    # 1st: matches for <p blah blah...> (like <p class =...>)
    # 2nd: matches for <p>
    # 3rd: matches for <p>blah blah...
    # Does the boolean operator OR work for grep??
    modLines <- c(grep("(^\\s*<p\\s+.*>$)", src),
                  grep("^\\s*<p>\\s*$", src),
                  grep("^\\s*<p>\\s*.+$", src))
    # Inserting special markers after the first ">"
    # Kind of protection to select the ">" after "<p"
    modLines <- gsub("(^.+.+?>)", "\\1~!@MARKER@!~", modLines)
    # Assuming below lines all end with ">"
    src[modLines] <- gsub(">", ' contenteditable="true">', src[modLines])
    
    # Load jQuery, ckeditor.js, annotator.js
    js <- readLines("edit.js")

    saver <- readLines("button.html")
    
    # Only one head tag per html document
    headLines <- grep("<head>", src)
    # Only one body tag per html document
    bodyLines <- grep("<body>", src)
    
    # 1st component: start to "<head>"
    # 2nd component: <head>+1 line to "<body>"
    # 3rd component: "<body>"+1 line to the end
    srcPieces <- list(src[1:headLines],
                      src[(headLines + 1):bodyLines],
                      src[(bodyLines + 1):length(src)])
    
    ################## Writing edit.html #######################################
    if (is.null(outfile)) {
        outfile <- gsub("post.html","edit.html", infile)
    }
    f <- file(outfile, open = "w")
    writeLines(c(srcPieces[[1]],
                 js,
                 srcPieces[[2]],
                 saver, srcPieces[[3]]), f)
    close(f)
}

