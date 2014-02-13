# load.dir is a function that converts annoying backslashes present in directories (for windows)
# into slashes.
# I wrote this just to use it in the other function, "source.edit". Otherwise, it doesn't have much use.

load.dir = function () {
  Dir.1 <- file.choose()
  if (grepl("[\\\\]", Dir.1) == TRUE) {
    gsub("\\\\","/", Dir.1)
  } else {
    print(Dir.1)
  }
}