# If "infile" input argument is not specified, file.choose() is run
# and the chosen file's directory is set as the working directory.
choose.file <- function() {
    infile <- file.choose()
    # Set the working directory to be the file chosen above
    # excluding the file name.
    # (matches for both windows and unix systems)
    setwd(gsub("(^.+)[\\\\+$].+$|(^.+)[/+$].+$","\\1\\2", infile))
    infile
}
