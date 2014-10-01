path <- "~/.dropbox/info.json"
c <- file(path, "r")
l <- suppressWarnings(readLines(c, -1L))
path_part <- sub(".*path\\\": \\\"","",l)
path <- sub("\\\",.*","",path_part)
path
