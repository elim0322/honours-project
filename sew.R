sew <- function(dir = NULL, out.dir = NULL) {
  if (is.null(dir)) {
    src.dir <- load.dir()
    src <- readLines(src.dir)
  } else src <- readLines(dir)
  
  R.begin <- grep("<!--begin.rcode", src)
  R.end <- c(grep("end.rcode-->", src), length(src))
  
  ############################# get rid of \t #############################
  for (i in 1:length(R.begin)) {
    if (!is.null(grep("\t", src[R.begin[i]:R.end[i]]))) {
      src <- gsub("\t", "  ", src)
    }
  }
  
  ######################### inline R code chunks  #########################
  in.line <- grep("<!--rinline", src)
  in.vector <- vector("numeric", length(in.line))
  
  for (i in 1:length(in.line)) {
    in.vector[i] <- gsub("^.+[<!--]rinline", "<!--keep.rinline", src[in.line][i])
    src[in.line][i] <- gsub("[-][>].*$", paste("->", in.vector[i]), src[in.line][i])
  }
  #########################################################################
  
  keep.list <- vector("list", (length(R.begin)+1))
  keep.vector <- vector("numeric",length(R.begin))
  
  for(i in 1:length(R.begin)) {
  keep.list[[i]] <- src[R.begin[i]:R.end[i]]
  keep.vector[i] <- gsub("[.]r", ".keep", keep.list[[i]][1])
  keep.list[[i]][1] <- keep.vector[i]
  }
  
  lines <- vector("numeric", length(R.begin)+1)
  lines[1] <- 1
  for (i in 1:(length(lines)-1)){
    lines[i+1] <- R.end[i]+1
  }
  
  if (is.null(out.dir)) {
    out.dir <- src.dir
    out.file <- gsub("Rhtml","post.Rhtml", out.dir)
  }
  
  f <- file(out.file, open = "w")
  for (i in 1:length(R.end)) {
    writeLines(c(src[lines[i]:R.end[i]], keep.list[[i]]), f)
  }
  close(f)
}
