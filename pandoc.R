# standardising (one-way: from Rmd -> Rmd)
system("pandoc -f markdown -t markdown -o example3.std.Rmd example3.Rmd")
# standardising (round trip: from Rmd -> HTML -> Rmd)
system("pandoc -f markdown -t html -o example3.html example3.Rmd")
system("pandoc -f html -t markdown -o example3.std.Rmd example3.html")
# the results are the same (ie, example.example3.std.Rmd == example3.std.Rmd)

# Problems with std.Rmd:
#  The meta data sections (syntax only for Rmd) such as:
#  ---
#  title: "Untitled"
#  output: html_document
#  ---
#  are completely ignored..
#
#  An R chunk with options:
#    ```{r, eval=F, echo=F, etc....}
#    1+1
#    ```
#  becomes:
#    `{r, echo=FALSE, eval=FALSE} 1+1`
#
#  An R chunk with multiple lines:
#  ```{r, echo=FALSE, eval=FALSE}
#    1+1
#    2+ 2
#    3 +3
#    4 + 4
#    ```
#  becomes:
#    `{r, echo=FALSE, eval=FALSE} 1+1 2+ 2 3 +3 4 + 4`
#  Impossible to tell where each line of R code is because of spaces..
#
#  An R chunk with no options:
#    ```{r}
#    1+1
#    ```
#  becomes:
#    ``` {.{r}}
#    summary(cars)
#    ```
#
#  Inline chunks (`r blah blah`) are not affected.
#
#  I decided to convert these chunks into comments in the HTML syntax because
#  once they are HTML comments they are not affected.
#  (maybe better to change specific types of R chunks to improve speed..)

# this is sew0() to preserve the metadata information..
# Rmd -> pre.Rmd
src <- readLines("example3.Rmd")
metadata <- grep("---", src) # metadata syntax is "---"
# there can be only one metadata chunk per document
src[metadata[1]] <- gsub("---", "<!-- metadata", src[metadata[1]])
src[metadata[2]] <- gsub("---", "metadata -->", src[metadata[2]])

# and to turn r code chunks into comments in HTML syntax.
chunksStart <- grep("^\\s*```+\\s*\\{r", src)
chunksEnd <- grep("^\\s*```+\\s*$", src)
chunksEnd2 <- vector("numeric", length=length(chunksStart))
for (i in 1:length(chunksStart)) {
    n <- min(which(chunksStart[i]<chunksEnd))
    chunksEnd2[i] <- chunksEnd[n]
}
#which.chunks <- chunksEnd2-chunksStart>2
chunks <- mapply(FUN=seq, chunksStart, chunksEnd2, SIMPLIFY=FALSE)
for (i in chunks) {
    src[i][1] <- gsub("```+", "<!--Rchunks.begin", src[i][1])
    last <- length(i)
    src[i][last] <- gsub("```+", "Rchunks.end-->", src[i][last])
}
writeLines(src, "example3.pre.Rmd")

#------------------------------------
# pre.Rmd -> std.Rmd
system("pandoc -f markdown -t markdown -o example3.std.Rmd example3.pre.Rmd")
#------------------------------------

# sew1: std.Rmd -> post.Rmd
src <- readLines("example3.std.Rmd")
# gsub R chunks and metadata chunks.
src <- gsub("<!--Rchunks.begin", "```", src)
src <- gsub("Rchunks.end-->", "```", src)
src <- gsub("<!-- metadata", "---", src)
src <- gsub("metadata -->", "---", src)
writeLines(src, "example3.post.Rmd")

#------------------------------------
# post.Rmd -> post.html
library(knitr)
rmarkdown::render("example3.post.Rmd")
#------------------------------------

#  <div>
#  <pre class="r"><code> ...R code... </code></pre>
#  <pre><code> </code></pre>
#  </div>







