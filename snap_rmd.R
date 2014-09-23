library(XML)
library(RCurl)
# The R script "choose_file.R" contains choose.file() that
# executes file.choose() and setwd() when infile is not specified.
source("choose_file.R")

# post-RMD.html -> edit-RMD.html
snap_rmd <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        # <<source("choose_file.R)">>
        infile <- choose.file()
    }
    # Check for correct infile format.
    if (!grepl("post-RMD.html$", infile)) {
        stop("infile must be a post-RMD.html file")
    }
    src <- readLines(infile)
    
    #--------------------- Add 'contenteditable="true"' ----------------------#
    html <- htmlParse(infile)
    # All the important info is included in a big div element:
    # '/html/body/div[@class="container-fluid main-container"]'
    
    # NOTE: post-RMD.html always has 4 top level elements: 
    #       style, div, and 2 scripts.
    # We can reliably use the top level div element as it has 
    # class="container-fluid main-container".
    
    # NOTE: <div id="TOC"> is created if toc: true and headings exist in Rmd.
    # Toc doesn't need to be editted. Only headings should be editted
    # and the round trip will create new toc based on new headers.
    # So toc section is left alone.
    # NOTE: rmarker::render() creates <div id="heading"> for a header named
    # "heading" and puts all underlying nodes in this div until next heading.
    # NOTE: R plots are created inside <p><img></img></p>.
    
    # ASSUMPTION: <pre> elements are only inserted by rmarkdown::render().
    # This looks like robust assumption as <pre> seems to be only used to 
    # contain R code chunks.
    
    # Find all node names inside /body/div/.
    mainXpath <- '//body/div[@class="container-fluid main-container"]'
    allNodes <- getNodeSet(html, paste0(mainXpath,"/*"))
    nodeNames <- sapply(allNodes, xmlName)
    
    # If there are headings in source doc, all nodes inside 
    # /body/div[@class="container-fluid..."] will be "div"s 
    # (first div is for the title and will contain <h1> nodes and
    # the rest is for different sections of headings).
    # If there are no headings, /body/div[class=...]/* nodes will be all
    # different.
    if (all((nodeNames)=="div")) {
        # If there are headings, get all nodes in //body/div/div except
        # pre (R stuff) and img (plots from R).
        # AND not the div for toc as there is no need to edit toc.
        node <- getNodeSet(html, paste0(mainXpath,
                '/div[not(@id="TOC")]/*[not(self::pre|img)]'))
    } else {
        # If there are no headings, find all nodes that are inside
        # "//body/" except <div id="header"> OR inside "//body/div/".
        # This Xpath is used to combine the right nodes into node, ie,
        # //body/div/* (heading) and /body/* (all other nodes).
        node <- getNodeSet(html, paste0(mainXpath, '/*[not(@id="header") and 
                not(self::pre|img)]', "|", mainXpath, '/div[@id="header"]/*'))
    }
    
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
        src[tag.lines] <- gsub("(^.*?<.*?)>", paste0(attr,'>'), src[tag.lines])
    }
    
    #---------------- Load jQuery, ckeditor.js, annotator.js -----------------#
    js <- readLines("edit.js")
    saver <- readLines("button.html")
    
    #------------------------ Generate pieces of src -------------------------#
    # NOTE: There are other scripts that interfere with our edit.js.
    #       A solution is to add our scripts the last.
    # Search for the closing tag </head>.
    headLines <- grep("</head>", src)
    # Only one body tag per html document
    bodyLines <- grep("<body.*>", src)
    
    # 1st component: start to "</head>"-1
    # 2nd component: </head> line to "<body>"
    # 3rd component: "<body>"+1 line to the end
    srcPieces <- list(src[1:headLines-1], src[(headLines):bodyLines],
                      src[(bodyLines + 1):length(src)])
    
    #-------------------------- Write edit-RMD.html --------------------------#
    if (is.null(outfile)) {
        outfile <- gsub("post-RMD.html","edit-RMD.html", infile)
    }
    f <- file(outfile, open = "w")
    writeLines(c(srcPieces[[1]], js, srcPieces[[2]], saver, srcPieces[[3]]), f)
    close(f)
}
