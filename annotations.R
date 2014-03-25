library(jsonlite)
library(XML)

########## Extract useful information from "test-annotations.txt" using json ##########
temp6 <- readLines("test-annotations.txt", warn = FALSE)
json <- fromJSON(temp6)

who <- json$rows$user
what <- json$row$quote
annotation <- json$rows$text
# "unlist" because src.json$rows$ranges is a list.
where <- unlist(json$rows$ranges)
where.start <- where[names(where) == "start"]
where.end <- where[names(where) == "end"]

anno.tags <- vector("list", length = length(what))
xpath.start <- vector("list", length = length(where.start))
xpath.end <- vector("list", length = length(where.end))
node.start <- vector("list", length = length(xpath.start))
node.end <- vector("list", length = length(xpath.end))
lineNumber <- vector("numeric", length = length(node.start))
html <- htmlParse("Attempt.edit.html")

for (i in 1:length(anno.tags)) {
    anno.tags[[i]] <- c('<p class="annotation">', 
                        paste("    The text", paste('"', what[i], '"', sep = ""),
                              "was annotated with the message", 
                              annotation[i], "by", paste('"', who[i], '"', sep = "")),
                        "</p>")
    if (length(xpath.start) == length(xpath.end)) {
        xpath.start[[i]] <- paste("/html/body/div[@class='chunk']", where.start[i], 
                                  "/ancestor::div[@class='chunk']", sep = "")
        xpath.end[[i]] <- paste("/html/body/div[@class='chunk']", where.end[i], 
                                "/ancestor::div[@class='chunk']", sep = "")
        node.start[[i]] <- getNodeSet(html, xpath.start[[i]])
        node.end[[i]] <- getNodeSet(html, xpath.end[[i]])
        
        # Can't compare XML node sets directly so compare their lengths.
        # Not robust here.
        #  If the lengths of the node sets are equal, then use the first
        #  element of node.start
        if (length(node.start[[i]]) == length(node.end[[i]])) {
            lineNumber[i] <- getLineNumber(html, node.start[[i]][[1]])
            else {
                # If their lengths are not the same, then use Which ever 
                # has the least number of elements...
                ################ Need Paul's help here ################
                lineNumber[i] <- getLineNumber(html, )
            }
        }
    }
}








# Just testing here.
a <- getNodeSet(html, "/html/body/div[@class='chunk']/div[1]/div[1]/pre[1]/span[7]/ancestor::div[@class='chunk']") 
getLineNumber("/html/body/div[@class='chunk']/div[1]/div[1]/pre[1]/span[7]/ancestor::div[@class='chunk']")
getLineNumber(a[[2]])