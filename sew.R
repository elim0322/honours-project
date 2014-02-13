sewr <- function(dir = NULL, out.dir = NULL) {
  if (is.null(dir)) {
    source.dir <- load.dir()
    source.doc <- readLines(source.dir)
  } else source.doc <- readLines(dir)
  
  R.begin <- grep("<!--begin.rcode", source.doc)
  R.end <- grep("end.rcode-->", source.doc)
  R.begin <- grep("<!--begin.keepcode", source.doc)
  R.end <- grep("end.rcode-->", source.doc)
  
  # Just checking
  if (length(R.begin) > length(R.end))
    stop("Please check as there might be extra '<!--begin.rcode' not being used")
  if (length(R.begin) < length(R.end))
    stop("Please check as there might be extra 'end.rcode-->' not being used")
    stop("Please check as there might be extra '<!--begin.keepcode' not being used")
  if (length(R.begin) < length(R.end))
    stop("Please check as there might be extra 'end.rcode-->' not being used")
  
  # Create an empty list and vector to store results from loop
  keep.list <- vector("list", length(R.begin))
  keep.vector <- vector("numeric",length(R.begin))
  
  for(i in 1:length(R.begin)) {
  keep.list[[i]] <- source.doc[R.begin[i]:R.end[i]]
  keep.vector[i] <- gsub("[.]r", ".keep", keep.list[[i]][1])
  keep.list[[i]][1] <- keep.vector[i]
  }
  
  # I tried to divide Rhtml into segments so that I can add bits and save the whole thing into post.Rhtml
  Rhtml.seg <- vector("list", length(R.begin)+1)
  Rhtml.seg[[1]] <- source.doc[1:R.end[1]]
  Rhtml.seg[[2]] <- source.doc[(R.end[1]+1):R.end[2]]
  Rhtml.seg[[3]] <- source.doc[(R.end[2]+1):length(source.doc)]
  
  result.list <- c(Rhtml.seg[[1]], keep.list[[1]], Rhtml.seg[[2]], keep.list[[2]], Rhtml.seg[3])
  result <- unlist(result.list)

  if (is.null(out.dir)) {
    out.dir <- source.dir
    out.file <- gsub("Rhtml","post.Rhtml", out.dir)
  } else {
    out.file <- out.dir
  }
    writeLines(result, out.file)
}
