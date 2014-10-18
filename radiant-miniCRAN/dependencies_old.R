# install packages required to run radiant
# next line checks if Radiant is being run locally or not
if(Sys.getenv('SHINY_PORT') == "") {

  if(file.exists("~/Desktop/GitHub/radiant_dev/") || file.exists("~/../Desktop/GitHub/radiant_dev/")) {

    pth <- normalizePath("~/Desktop/GitHub/radiant_dev/radiant-miniCRAN", winslash = "/")

  } else {

    if (.Platform$OS.type == 'windows') {
      Sys.setlocale(category = 'LC_ALL','English_United States.1252')
      fpath <- paste0(Sys.getenv('APPDATA'),"/Dropbox/info.json")
    } else {
      Sys.setlocale(category = 'LC_ALL','en_US.UTF-8')
      fpath <- '~/.dropbox/info.json'
    }

    f <- file(fpath,'r');
    json_string <- suppressWarnings(readLines(f, -1L));
    close(f)
    path_part <- sub('.*path\\\": \\\"','',json_string);
    pth <- paste0(sub('\\\",.*','',path_part), '/radiant');

    if(file.exists(pth)) {
      cat(paste('radiant folder found in', pth,''))
    } else {
      cat('No radiant folder found in your Dropbox. Did you accept the invitation to share the radiant folder?')
      q('ask')
    }

    # setting the location of the miniCRAN
    pth <- normalizePath(paste0(pth,"/radiant-miniCRAN"), winslash = "/")
  }

  # install to user directory
  local_dir <- Sys.getenv("R_LIBS_USER")
  if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

  # loading the list of pkgs needed to run radiant
  source(paste0(pth,"/pkgs.R"))

  # setting the url for the miniCRAN
  mcran <- paste0("file:///",pth)

  # look locally first and then in the Rstudion CRAN
  # options(repos = c(CRAN = c(mcran,"http://cran.rstudio.com")))
  options(repos = c(CRAN = mcran))

  # udpate old-packages
  update.packages(lib.loc = local_dir, ask = FALSE)

  # install packages that are available but were not installed before
  to_inp <- new.packages(lib.loc = local_dir)

#   if(length(to_inp) != 0) install.packages(to_inp, lib.loc = local_dir)
  if(length(to_inp) != 0) install.packages(to_inp, local_dir)

  # add function to remove packages that are not used by Radiant?
  # might mess up someones stuff. maybe better to do this like packRat does it
  # i.e., install into a non-standard directory

  # load/attach packages
#   suppressWarnings(sapply(rownames(installed.packages()), require, lib.loc = local_dir, character.only=TRUE))
#   if (.Platform$OS.type == 'windows') remove.packages('Cairo')

  suppressWarnings(sapply(pkgs[pkgs != "R.utils"], require, lib.loc = local_dir, character.only=TRUE))

} else {
  # when run on Shiny-server make sure you have install the packages already
  # using sudo su
#   mcran <- "file:///home/vnijs/Desktop/radiant/radiant-miniCRAN"
#   options(repos = c(CRAN = mcran))
#
#   update.packages(lib.loc = local_dir, ask = FALSE)
#   to_inp <- new.packages(lib.loc = local_dir)
#   if(length(to_inp) != 0) install.packages(to_inp, local_dir)
    # install to user directory
  local_dir <- Sys.getenv("R_LIBS_USER")
  if(!file.exists(local_dir)) dir.create(local_dir, recursive = TRUE)

  # loading the list of pkgs needed to run radiant
  source(paste0(pth,"/pkgs.R"))

  # setting the url for the miniCRAN
  mcran <- paste0("file:///",pth)

  # look locally first and then in the Rstudion CRAN
  # options(repos = c(CRAN = c(mcran,"http://cran.rstudio.com")))
  options(repos = c(CRAN = mcran))

  # udpate old-packages
  update.packages(lib.loc = local_dir, ask = FALSE)

  # install packages that are available but were not installed before
  to_inp <- new.packages(lib.loc = local_dir)

#   if(length(to_inp) != 0) install.packages(to_inp, lib.loc = local_dir)
  if(length(to_inp) != 0) install.packages(to_inp, local_dir)

  # add function to remove packages that are not used by Radiant?
  # might mess up someones stuff. maybe better to do this like packRat does it
  # i.e., install into a non-standard directory

  # load/attach packages
#   suppressWarnings(sapply(rownames(installed.packages()), require, lib.loc = local_dir, character.only=TRUE))
#   if (.Platform$OS.type == 'windows') remove.packages('Cairo')

  suppressWarnings(sapply(pkgs[pkgs != "R.utils"], require, lib.loc = local_dir, character.only=TRUE))

}
