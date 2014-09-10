library(XML)
library(RCurl)
source("choose_file.R")

# post.html -> edit.html
snap_rmd <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
    if (!grepl("post-md.html$", infile)) {
        stop("infile must be a post-md.html file")
    }
    src <- readLines(infile)
    
    #-------------------- Add 'contenteditable="true"' --------------------#
    html <- htmlParse(infile)
    # All the important info is included in a big div element:
    # 'body/div[@class="container-fluid main-container"]'
    # Set main path.
    mainXpath <- '//body/div[@class="container-fluid main-container"]'
    
    # Node for HTML title.
    # NOTE: <div id="TOC"> is created if toc: true in source Rmd doc.
    # Toc doesn't need to be editted. Only headers should be editted
    # and the round trip will create new toc based on new headers.
    # So toc section is left alone.
    title <- paste0(mainXpath, '/div[@id="header"]/*')
    
    # Node for actual text (This is assuming that <pre> elements are only 
    # inserted by rmarkdown::render. It looks like robust assumption as <pre>
    # seems to be only used to contain R code chunks).
    # In addition, <img> is excluded which is produced from R chunks that run plot().
    # NOTE: rmarker::render() creates <div id="header-1"> IF header is in source
    # doc and puts all <p>'s inside that div.
    # main <- paste0(mainXpath, '/div[@id="header-1"]/*[not(self::pre|img)]')
    main <- paste0(mainXpath, '/*[not(self::pre|img|div)]')
    # Paste title and main by boolean OR separator.
    node <- getNodeSet(html, paste(title,main,sep="|"))
    getNodeSet(html, main)
    getNodeSet(html, title)
    main <- paste0(mainXpath, '/*[not(self::div|img|pre)]')
    
    for (i in 1:length(node)) {
        tag.lines <- getLineNumber(node[[i]])
        id.attr <- xmlGetAttr(node[[i]], "id")
        # if there is no id attribute, insert one
        # otherwise just add contenteditable="true" attr
        attr <- '\\1 contenteditable=\"true\"'
        if (is.null(id.attr)) {
            # Generate id attributes
            editorID <- paste("id=", '\"Editor-', i, '\"', sep="")
            attr <- paste(attr, editorID)
        }
        # Search for "<tag...>" and replace the first ">" with
        #  'contenteditable="true"'
        src[tag.lines] <- gsub("(^.*?<.*?)>", 
                               paste0(attr, '>'), src[tag.lines])
    }
    
    #-- Load jQuery, ckeditor.js, annotator.js --#
    js <- readLines("edit.js")
    saver <- readLines("button.html")
    
    #------------ Generate pieces of src -------------#
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
    
    #--------------- Writing edit.html ---------------#
    if (is.null(outfile)) {
        outfile <- gsub("post-md.html","edit-md.html", infile)
    }
    f <- file(outfile, open = "w")
    writeLines(c(srcPieces[[1]],
                 js,
                 srcPieces[[2]],
                 saver, srcPieces[[3]]), f)
    close(f)
}
