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

############ Create empty vectors and lists to be used in for loops later #############
anno.tags <- vector("list", length = length(what))
xpath.start <- vector("list", length = length(where.start))
xpath.end <- vector("list", length = length(where.end))
node.start <- vector("list", length = length(xpath.start))
node.end <- vector("list", length = length(xpath.end))
lineNumber <- vector("numeric", length = length(node.start))

# Create a copy of "Attempt.edit.html" and save it as "Attempt.save.html"
#  so I can write lines into "Attempt.save.html" and carry out other
#  stuff in the for loop below:
src <- readLines("Attempt.edit.html")
f <- file("Attempt.save.html", open = "w")
writeLines(src, f)
close(f)

for (i in 1:length(anno.tags)) {
    # Create annotation tags.
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
        html <- htmlParse("Attempt.save.html")
        node.start[[i]] <- getNodeSet(html, xpath.start[[i]])
        node.end[[i]] <- getNodeSet(html, xpath.end[[i]])
        lineNumber[i] <- getLineNumber(node.start[[i]][[1]])
        src <- readLines("Attempt.save.html", warn = FALSE)
        src <- append(src, anno.tags[[i]], after = lineNumber[i]-1)
        f <- file("Attempt.save.html", open = "w")
        writeLines(src, f)
        close(f)
    }
}

        # Can't compare XML node sets directly so compare their lengths.
        # Not robust here.
        #  If the lengths of the node sets are equal, then use the first
        #  element of node.start
        #if (length(node.start[[i]]) == length(node.end[[i]])) {
        #    lineNumber[i] <- getLineNumber(html, node.start[[i]][[1]])
        #    else {
                # If their lengths are not the same, then use Which ever 
                # has the least number of elements...
                ################ Need Paul's help here ################
        #        lineNumber[i] <- getLineNumber(html, )
            }
        }
    }
}