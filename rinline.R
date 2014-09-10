copyInline <- function(line, format) {
  # NOTE that the regular expression below is the "official"
  # pattern for knitr inline R code (all_patterns$html$inline.code)
  if (format=="html") {
    temp <- gsub("<!--\\s*rinline", "<!--rinline.Rhtml_keep", line)
  } else if (format=="md") {
    temp <- gsub("(`)r( +[^`]+\\s*)(`)", "<!--rinline.Rmd_keep\\2-->", line)
  }
  paste(line, temp, sep = "")
}

processRinline <- function(line, format) {
  # To use the official pattern (eg, all_patterns$html$inline.code)
  pattern <- paste0("all_patterns$", format, "$inline.code")
  # For example, pattern is: <!--\\s*rinline(.+?)-->.
  # Make the pattern to be (<!--\\s*rinline(.+?)-->).
  regexp <- paste0("(", eval(parse(text=pattern)), ")")
  # Using "mark" to retain text in front and at the end of inline chunks.
  mark <- gsub(regexp, "~MARKER~\\1~MARKER~", line)
  temp <- unlist(strsplit(mark, "~MARKER~"))
  lines <- grep("(^.*<!--\\s*rinline.+$)|(`r +.*`)", temp)
  for (i in 1:length(lines)) {
    temp[lines][i] <- copyInline(temp[lines][i], format)
  }
  result <- paste(temp, collapse = "")
  result
}

