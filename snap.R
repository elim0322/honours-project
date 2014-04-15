library(XML)
snap <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        infile <- load.dir()
    }
    if (!grepl("post.html$", infile))
        stop("infile is not a post.html file")
    
    src <- readLines(infile)
    
    ###################################
    html <- htmlParse(infile)
    # Select all <p> tags regardless of their locations.
    node <- getNodeSet(html, "//p")
    for (i in 1:length(node)) {
        tag.lines <- getLineNumber(node[[i]])
        # Search for "<p...>" and replace the first ">" with
        #  'contenteditable="true"'
        src[tag.lines] <- gsub("(^.*<.*p.*).*?>",
                               '\\1 contenteditable="true">', src[tag.lines])
    }
    
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

