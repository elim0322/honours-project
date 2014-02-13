# source.edit is a function that replaces unwanted texts/typos in source document with desired texts,
# and saves the output.
#
# Arguments:
#   
#   old.dir = source document directory 
#     (if left empty, file.choose() window opens to select the source document file)
#   
#   new.dir = directory with desired file name for the output 
#     (if left empty, output is saved into the directory of the source document with "New" added to the file name)
#
#   old.txt = text that requires editting
#
#   new.txt = text that is used as an edit

source.edit = function (old.dir = NULL, new.dir = NULL, old.txt, new.txt) {
  if (is.null(old.dir)) {
    source.dir <- load.dir()
    sourceDoc <- readLines(source.dir)
  } else sourceDoc <- readLines(old.dir)
  
  if (is.null(new.dir)) {
    output.dir <- source.dir
    output.file <- gsub('^(.+[/])','\\1New \\2',output.dir)
  } else {
    output.file = new.dir
  }
  
  new.sourceDoc <- gsub(old.txt, new.txt, sourceDoc)
  writeLines(new.sourceDoc, output.file)
}


# example:
source.edit(old.txt = "Argument", new.txt = "(HERE)")