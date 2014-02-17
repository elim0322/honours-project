load.dir = function () {
  dir.1 <- file.choose()
  if (grepl("[\\\\]", dir.1) == TRUE) gsub("\\\\","/", dir.1)
  else dir.1 <- dir.1
}
