# install packages required to run radiant
# next line checks if Radiant is being run locally or not
if(Sys.getenv('SHINY_PORT') == "") {

  if(file.exists("~/Desktop/radiant_dev/")) {
    mcran <- paste0("file:///",normalizePath("~/Desktop/radiant_dev/radiant-miniCRAN", winslash = "/"))
  } else {

    if (.Platform$OS.type == 'windows') {
      fpath <- Sys.getenv('APPDATA')
    } else {
      fpath <- '~/.dropbox/info.json'
    }

    f <- file(fpath,'r');
    json_string <- suppressWarnings(readLines(f, -1L));
    path_part <- sub('.*path\\\": \\\"','',json_string);
    pth <- paste0(sub('\\\",.*','',path_part), '/radiant');
    setwd(normalizePath(pth, winslash='/'));

    if(file.exists(pth)) {
      cat(paste('radiant folder found in', pth,''))
      setwd(pth)
    } else {
      cat('No radiant folder found in your Dropbox. Did you accept the invitation to share the radiant folder?')
      q('ask')
    }
  }

  # look locally first and then in the Rstudion CRAN
  # options(repos = c(CRAN = c(mcran,"http://cran.rstudio.com")))
  options(repos = c(CRAN = mcran))

  # install to user directory
  local_dir <- Sys.getenv("R_LIBS_USER")
  if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

  # loading the list of pkgs needed to run radiant
  source(paste0(mcran,"/pkgs.R"))

  # check if packages in pkgs are alraedy installed locally
  available <- suppressWarnings(sapply(pkgs, require, lib.loc = local_dir, character.only=TRUE))
  inst.libs <- pkgs[available == FALSE]

  # install and require the packages that have not yet been installed
  if(length(inst.libs) != 0) {
    install.packages(inst.libs, local_dir)
    suppressWarnings(sapply(inst.libs, require, lib.loc = local_dir, character.only=TRUE))
  }

} else {
  # when run on Shiny-server make sure you have install the packages already
  # using sudo su
  mcran <- "file:///home/vnijs/Desktop/radiant/radiant-miniCRAN"
  source(paste0(mcran,"/pkgs.R"))
  sapply(pkgs, require, character.only=TRUE)
}
