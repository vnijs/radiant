# From Andrei's miniCRAN package
.makeRepo <- function(pkgs, path, repos=getOption("repos"), type="source",
                     Rversion=R.version, download=TRUE, localdir="", writePACKAGES=TRUE){
  if(!file.exists(path)) stop("Download path does not exist")
  Rversion <- .twodigitRversion(Rversion)

  folder <- .repoPrefix(type, Rversion)
  pkgPath <- file.path(path, folder)
  if(!file.exists(pkgPath)) {
    result <- dir.create(pkgPath, recursive=TRUE)
    if(result) {
      message("Created new folder: ", pkgPath)
    } else {
      stop("Unable to create repo path: ", pkgPath)
    }
  }

  if(localdir != "") {
    file.copy(list.files(paste0(localdir,"/",type), full.names=TRUE),pkgPath)
    download <- FALSE
  }
  if(download) download.packages(pkgs, destdir=pkgPath, repos=repos, type=type)
  if(type %in% c("mac.binary.mavericks", "mac.binary.leopard")) type <- "mac.binary"
  if(writePACKAGES) tools::write_PACKAGES(dir=pkgPath, type=type)
}

# not changed from original
.repoPrefix <- function(type, Rversion){
  switch(
    type,
    "source" = "src/contrib",
    "win.binary" = sprintf("bin/windows/contrib/%s", Rversion),
    "mac.binary" = sprintf("bin/macosx/contrib/%s", Rversion),
    "mac.binary.mavericks" =  sprintf("bin/macosx/mavericks/contrib/%s", Rversion),
    "mac.binary.leopard"= sprintf("bin/macosx/leopard/contrib/%s", Rversion),
    stop("Type ", type, "not recognised.")
  )
}

# not changed from original
.twodigitRversion <- function(R = R.version){
 paste(R$major, strsplit(R$minor, ".", fixed = TRUE)[[1L]][1L], sep = ".")
}
