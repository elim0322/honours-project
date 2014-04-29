library(XML)
library(httr)
library(RCurl)

snap <- function(infile = NULL, outfile = NULL, upload = TRUE) {
    if (is.null(infile)) {
        infile <- load.dir()
    }
    if (!grepl("post.html$", infile))
        stop("infile is not a post.html file")
    
    src <- readLines(infile)
    
    ####################### Add 'contenteditable="true"' #######################
    html <- htmlParse(infile)
    # Select all the child nodes of the body element (i.e. all top level
    #  elements inside body tags) that do not have "class='chunk'" attributes.
    node <- getNodeSet(html, '//body/*[not(@class="chunk")]')
    
    for (i in 1:length(node)) {
        tag.lines <- getLineNumber(node[[i]])
        # Search for "<p...>" and replace the first ">" with
        #  'contenteditable="true"'
        src[tag.lines] <- gsub("(^.*<.*).*?>",
                               '\\1 contenteditable="true">', src[tag.lines])
    }
    
    ################## Load jQuery, ckeditor.js, annotator.js ##################
    js <- readLines("edit.js")
    saver <- readLines("button.html")
    
    ########################## Generate pieces of src ##########################
    # Only one head tag per html document
    headLines <- grep("<head>", src)
    # Only one body tag per html document
    bodyLines <- grep("<body.*>", src)
    
    # 1st component: start to "<head>"
    # 2nd component: <head>+1 line to "<body>"
    # 3rd component: "<body>"+1 line to the end
    srcPieces <- list(src[1:headLines],
                      src[(headLines + 1):bodyLines],
                      src[(bodyLines + 1):length(src)])
    
    ############################ Writing edit.html #############################
    if (is.null(outfile)) {
        outfile <- gsub("post.html","edit.html", infile)
    }
    f <- file(outfile, open = "w")
    writeLines(c(srcPieces[[1]],
                 js,
                 srcPieces[[2]],
                 saver, srcPieces[[3]]), f)
    close(f)
    
    ################################ FROM PAUL #################################
    # Submitting file to be edited (instead of using upload.html via browser)
    if (upload) {
        POST("http://stat220.stat.auckland.ac.nz/cke/upload.php",
             body=list(MAX_FILE_SIZE="40000", userfile=fileUpload(outfile)),
             config=list(userpwd="cke:cke", httpauth=1L, verbose=TRUE))
    }
}
