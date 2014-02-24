rip <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
      infile <- load.dir()
    } 
    if (!grepl("html$", infile))
      stop("infile is not an html file")
    
    library(XML)
    html <- htmlParse(infile)
    
    ################# Removing div class = "chunk" #################
    chunks <- getNodeSet(html, "//div[@class = 'chunk']")
    for (i in 1:length(chunks)) {
      removeChildren(xmlParent(chunks[[i]]), kids = chunks[i])
    }
    
    ##################### Removing knitr style #####################
    knitr <- getNodeSet(html, "//style[@type = 'text/css']")
    for (i in 1:length(knitr)) {
      removeChildren(xmlParent(knitr[[i]]), kids = knitr[i])
    }
    
    if (is.null(outfile)) {
      outfile <- gsub("post.html","return.Rhtml", infile)
    }
    saveXML(html, outfile)
}
