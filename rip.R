library(XML)

rip <- function(infile = NULL, outfile = NULL) {
    if (is.null(infile)) {
        infile <- load.dir()
    } 
    if (!grepl("html$", infile))
        stop("infile is not an html file")
    
    html <- htmlParse(infile)
    
    ################# Removing div class = "chunk" #################
    chunks <- getNodeSet(html, "//div[@class = 'chunk']")
    for (i in 1:length(chunks)) {
        removeChildren(xmlParent(chunks[[i]]), kids = chunks[i])
    }
    
    ##################### Removing knitr style #####################
    knitr <- getNodeSet(html, "//style[@type = 'text/css']")
    for (i in 1:length(knitr)) {
        style <- knitr[[i]]
        # ONLY remove <style> if knitr put it there.
        # Determine whether <style> was put there by knitr by
        # looking for .knitr.inline rule within <style> content.
        if (grepl(".knitr.inline", xmlValue(style))) {
            removeChildren(xmlParent(style), kids = knitr[i])
        }
    }
    
    if (is.null(outfile)) {
        outfile <- gsub("post.html","return.Rhtml", infile)
    }
    saveXML(html, outfile)
}
