# copyInline() copies an inline chunk and pastes the copy with the original.
# NOTE:
#  Due to difference in the regular expressions for 'html' and 'md', md inline
#  chunk copies do not retain empty spaces (ie, `r 1+2` and `r  1+2' all result
#  in <!--rinline.keep1+2-->), whereas 'html' copies will.
#  Just need to be aware of this for rip().
copyInline <- function(line, format) {
    # Official pattern (all_patterns).
    patternString <- paste0("all_patterns$", format, "$inline.code")
    patternRE <- eval(parse(text=patternString))
    # Create a regular expression to copy.
    copyRE <- paste0("^.*", patternRE, ".*$")
    copy <- gsub(copyRE, "<!--rinline.keep\\1-->", line)
    # Create a regular expression to extract the 
    # original inline code.
    originalRE <- paste0("^.*(", patternRE, ").*$")
    original <- gsub(originalRE, "\\1", line)
    # Paste the original and copy together.
    paste(original, copy, sep = "")
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
  lines <- grep(regexp, temp)
  for (i in 1:length(lines)) {
    temp[lines][i] <- copyInline(temp[lines][i], format)
  }
  result <- paste(temp, collapse = "")
  result
}

