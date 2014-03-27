library(jsonlite)
library(XML)

########## Extract useful information from "test-annotations.txt" using json ##########
temp6 <- readLines("test-annotations.txt", warn = FALSE)
json <- fromJSON(temp6, simplifyVector = FALSE)

numAnn <- length(json$rows)

    # Create a copy of "Attempt.edit.html" and save it as "Attempt.save.html"
    #  so I can write lines into "Attempt.save.html" and carry out other
    #  stuff in the for loop below:
file.copy("Attempt.edit.html", "Attempt.save.html")

for (i in 1:numAnn) {
    ann <- json$rows[[i]]
    who <- ann$user
    what <- ann$quote
    annotation <- ann$text
    # ASSUME only one ranges value
    where <- ann$ranges[[1]]  
    where.start <- where$start
    where.end <- where$end

    # Create annotation tags.
    anno.tags <- c('<p class="annotation">', 
                   paste("    The text \"", what, 
                         "\" was annotated with the message \"", 
                         annotation, "\" by \"", who, "\"", sep=""),
                   "</p>")
    xpath.start <- paste("/html/body/div[@class='chunk']", where.start, 
                         "/ancestor::div[@class='chunk']", sep = "")
    xpath.end <- paste("/html/body/div[@class='chunk']", where.end, 
                       "/ancestor::div[@class='chunk']", sep = "")
    html <- htmlParse("Attempt.save.html")
    node.start <- getNodeSet(html, xpath.start)
    node.end <- getNodeSet(html, xpath.end)
    # Improve next line to decide whether to use [[1]] or [[2]] or ...
    lineNumber <- getLineNumber(node.start[[1]])
    src <- readLines("Attempt.save.html", warn = FALSE)
    src <- append(src, anno.tags, after = lineNumber[i]-1)
    writeLines(src, "Attempt.save.html")
}

 
